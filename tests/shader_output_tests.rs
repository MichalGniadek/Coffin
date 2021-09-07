use globset::Glob;
use image::io::Reader as ImageReader;
use image::{ImageBuffer, Rgba};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use vulkano::{
    buffer::*, command_buffer::*, descriptor::descriptor_set::*, descriptor::*, device::*,
    format::*, image::*, instance::*, pipeline::*, sync::*,
};

#[test]
fn compute() {
    let glob = Glob::new("*.coff").unwrap().compile_matcher();
    let mut test_folder = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    test_folder.push("tests/shaders");

    for entry in test_folder.read_dir().unwrap() {
        let entry = entry.unwrap();

        if entry.file_type().unwrap().is_file() && glob.is_match(entry.file_name()) {
            let src = coffin2::compile_file(&entry.path()).unwrap();
            if let Err(err) = coffin2::validate_spirv(&src) {
                panic!("{:#?}", err);
            }
            let new_image = test_shader(&src).unwrap();

            let img_path = entry.path().with_extension("png");
            let old_image = match ImageReader::open(&img_path) {
                Ok(img) => match img.decode().unwrap() {
                    image::DynamicImage::ImageRgba8(img) => img,
                    _ => unimplemented!(),
                },
                Err(_) => {
                    new_image.save(img_path.as_path()).unwrap();
                    continue;
                }
            };

            if new_image != old_image {
                new_image
                    .save(img_path.with_extension("new.png").as_path())
                    .unwrap();
                panic!(
                    "Different images from '{}'",
                    entry.file_name().to_str().unwrap()
                );
            }
        }
    }
}

#[allow(unsafe_code)]
mod vulkan_img_shader {
    use std::{ffi::CStr, sync::Arc};
    use vulkano::{
        descriptor::{descriptor::*, pipeline_layout::*},
        device::Device,
        pipeline::shader::*,
        OomError,
    };

    pub struct Shader {
        shader: Arc<ShaderModule>,
    }

    impl Shader {
        pub fn load(device: Arc<Device>, words: &[u32]) -> Result<Shader, OomError> {
            unsafe {
                Ok(Shader {
                    shader: ShaderModule::from_words(device, words)?,
                })
            }
        }

        pub fn main_entry_point(&self) -> ComputeEntryPoint<(), Layout> {
            unsafe {
                self.shader.compute_entry_point(
                    CStr::from_bytes_with_nul(b"main\0").unwrap(),
                    Layout(ShaderStages {
                        compute: true,
                        ..ShaderStages::none()
                    }),
                )
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Layout(pub ShaderStages);

    unsafe impl PipelineLayoutDesc for Layout {
        fn num_sets(&self) -> usize {
            1
        }

        fn num_bindings_in_set(&self, set: usize) -> Option<usize> {
            match set {
                0 => Some(1),
                _ => None,
            }
        }

        fn descriptor(&self, set: usize, binding: usize) -> Option<DescriptorDesc> {
            match (set, binding) {
                (0, 0) => Some(DescriptorDesc {
                    ty: DescriptorDescTy::Image(DescriptorImageDesc {
                        sampled: false,
                        dimensions: DescriptorImageDescDimensions::TwoDimensional,
                        format: None,
                        multisampled: false,
                        array_layers: DescriptorImageDescArray::NonArrayed,
                    }),
                    array_count: 1,
                    stages: self.0.clone(),
                    readonly: true,
                }),
                _ => None,
            }
        }

        fn num_push_constants_ranges(&self) -> usize {
            0
        }

        fn push_constants_range(&self, _: usize) -> Option<PipelineLayoutDescPcRange> {
            None
        }
    }
}

fn test_shader(src: &[u32]) -> Result<ImageBuffer<Rgba<u8>, Vec<u8>>, Box<dyn Error>> {
    let instance = Instance::new(None, &InstanceExtensions::none(), None)?;
    let physical = PhysicalDevice::enumerate(&instance).next().unwrap();
    let queue_family = physical
        .queue_families()
        .find(|&q| q.supports_compute())
        .unwrap();

    let (device, mut queues) = {
        Device::new(
            physical,
            &Features::none(),
            &DeviceExtensions::none(),
            [(queue_family, 0.5)].iter().cloned(),
        )?
    };

    let queue = queues.next().unwrap();

    let image = StorageImage::new(
        device.clone(),
        Dimensions::Dim2d {
            width: 1024,
            height: 1024,
        },
        Format::R8G8B8A8Unorm,
        Some(queue.family()),
    )?;

    let shader = vulkan_img_shader::Shader::load(device.clone(), src)?;

    let compute_pipeline = Arc::new(ComputePipeline::new(
        device.clone(),
        &shader.main_entry_point(),
        &(),
        None,
    )?);

    let set = Arc::new(
        PersistentDescriptorSet::start(
            compute_pipeline
                .layout()
                .descriptor_set_layout(0)
                .unwrap()
                .clone(),
        )
        .add_image(image.clone())?
        .build()?,
    );

    let buf = CpuAccessibleBuffer::from_iter(
        device.clone(),
        BufferUsage::all(),
        false,
        (0..(1024 * 1024 * 4)).map(|_| 0u8),
    )?;

    let mut builder = AutoCommandBufferBuilder::new(device.clone(), queue.family())?;
    builder
        .dispatch(
            [1024 / 8, 1024 / 8, 1],
            compute_pipeline.clone(),
            set.clone(),
            (),
        )?
        .copy_image_to_buffer(image.clone(), buf.clone())?;
    let command_buffer = builder.build()?;

    command_buffer
        .execute(queue.clone())?
        .then_signal_fence_and_flush()?
        .wait(None)?;

    let buf = buf.read()?.to_owned();
    Ok(ImageBuffer::<Rgba<u8>, Vec<u8>>::from_raw(1024, 1024, buf).unwrap())
}
