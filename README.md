# Multilayer Networks for Neuromodulation

This project is developed within the framework of META-BRAIN (2026).
Brain stimulation induces changes that span multiple levels of neural organisation, from cellular activity and local circuits to large-scale brain networks. The aim of this study is to quantify these longitudinal changes using graph theory and network-based analyses.
To achieve this, we are developing a multimodal network analysis pipeline based on the publicly available OPEN-DBS dataset from OpenNeuro. The workflow integrates structural and diffusion MRI data (T1-weighted and diffusion-weighted imaging) acquired across multiple postoperative timepoints.

Following image preprocessing, modality-specific analyses are performed, including:

- Subcortical volumetric analysis
- Cortical thickness analysis
- Structural connectome construction from diffusion MRI tractography

Brain regions are defined using the Desikan–Killiany cortical atlas and FreeSurfer aseg subcortical segmentation. Outputs from each modality are then combined into a multilayer network framework, enabling the investigation of DBS-related structural and connectivity changes across time.
The final objective is to characterise network reorganisation following deep brain stimulation and to establish a reproducible methodology for multimodal longitudinal network analysis in neuromodulation studies.
