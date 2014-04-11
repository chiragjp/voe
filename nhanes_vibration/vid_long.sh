bsub -q long -W 48:00 -J LBXVID_8_s0 -o ../../params/LBXVID_8_s0.out Rscript vibrationAnalysisK.R -p ../../params/LBXVID_8_s0_param.Rdata
bsub -q long -W 48:00 -J LBXVID_9_s0 -o ../../params/LBXVID_9_s0.out Rscript vibrationAnalysisK.R -p ../../params/LBXVID_9_s0_param.Rdata
bsub -q long -W 48:00 -J LBXVID_10_s0 -o ../../params/LBXVID_10_s0.out Rscript vibrationAnalysisK.R -p ../../params/LBXVID_10_s0_param.Rdata
bsub -q long -W 48:00 -J LBXVID_11_s0 -o ../../params/LBXVID_11_s0.out Rscript vibrationAnalysisK.R -p ../../params/LBXVID_11_s0_param.Rdata