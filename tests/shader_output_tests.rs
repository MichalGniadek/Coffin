use globset::Glob;
use image::io::Reader as ImageReader;
use image::{ImageBuffer, Rgba};
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
            let new_image = test_shader(&src);

            let img_path = entry.path().with_extension("png");
            let old_image = match ImageReader::open(&img_path) {
                Ok(img) => match img.decode().unwrap() {
                    image::DynamicImage::ImageRgba8(img) => img,
                    _ => panic!(),
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
                    shader: ShaderModule::from_words(device, &words)?,
                })
            }
        }

        pub fn main_entry_point(&self) -> ComputeEntryPoint<(), Layout> {
            unsafe {
                static NAME: [u8; 5usize] = [109u8, 97u8, 105u8, 110u8, 0];
                self.shader.compute_entry_point(
                    CStr::from_ptr(NAME.as_ptr() as *const _),
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
            1usize
        }

        fn num_bindings_in_set(&self, set: usize) -> Option<usize> {
            match set {
                0usize => Some(1usize),
                _ => None,
            }
        }

        fn descriptor(&self, set: usize, binding: usize) -> Option<DescriptorDesc> {
            match (set, binding) {
                (0usize, 0usize) => Some(DescriptorDesc {
                    ty: DescriptorDescTy::Image(DescriptorImageDesc {
                        sampled: false,
                        dimensions: DescriptorImageDescDimensions::TwoDimensional,
                        format: None,
                        multisampled: false,
                        array_layers: DescriptorImageDescArray::NonArrayed,
                    }),
                    array_count: 1u32,
                    stages: self.0.clone(),
                    readonly: true,
                }),
                _ => None,
            }
        }

        fn num_push_constants_ranges(&self) -> usize {
            0usize
        }

        fn push_constants_range(&self, num: usize) -> Option<PipelineLayoutDescPcRange> {
            if num != 0 || 0usize == 0 {
                None
            } else {
                Some(PipelineLayoutDescPcRange {
                    offset: 0,
                    size: 0usize,
                    stages: ShaderStages::all(),
                })
            }
        }
    }
}

fn test_shader(src: &[u32]) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    let instance =
        Instance::new(None, &InstanceExtensions::none(), None).expect("failed to create instance");

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
        )
        .unwrap()
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
    )
    .unwrap();

    let shader = vulkan_img_shader::Shader::load(device.clone(), src).unwrap();

    let compute_pipeline = Arc::new(
        ComputePipeline::new(device.clone(), &shader.main_entry_point(), &(), None).unwrap(),
    );

    let set = Arc::new(
        PersistentDescriptorSet::start(
            compute_pipeline
                .layout()
                .descriptor_set_layout(0)
                .unwrap()
                .clone(),
        )
        .add_image(image.clone())
        .unwrap()
        .build()
        .unwrap(),
    );

    let buf = CpuAccessibleBuffer::from_iter(
        device.clone(),
        BufferUsage::all(),
        false,
        (0..1024 * 1024 * 4).map(|_| 0u8),
    )
    .unwrap();

    let mut builder = AutoCommandBufferBuilder::new(device.clone(), queue.family()).unwrap();
    builder
        .dispatch(
            [1024 / 8, 1024 / 8, 1],
            compute_pipeline.clone(),
            set.clone(),
            (),
        )
        .unwrap()
        .copy_image_to_buffer(image.clone(), buf.clone())
        .unwrap();
    let command_buffer = builder.build().unwrap();

    let finished = command_buffer.execute(queue.clone()).unwrap();

    finished
        .then_signal_fence_and_flush()
        .unwrap()
        .wait(None)
        .unwrap();

    let buf = buf.read().unwrap().to_owned();
    ImageBuffer::<Rgba<u8>, Vec<u8>>::from_raw(1024, 1024, buf).unwrap()
}
