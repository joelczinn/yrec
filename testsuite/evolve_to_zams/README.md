# Zero age main sequence (ZAMS) models

| Model Name     | Description    |
| ------------ | -------------- |
| `Test_m0100_feh0_base_ZAMS`      | A 0.1$M_\odot$ model taken to be the "base case". No diffusion, no rotation, and with a gray atmosphere, run to the ZAMS. |
| `Test_m0100_feh0_allard_ZAMS`      | A 0.1$M_\odot$ base model with the Allard model atmosphere (`KTTAU = 4`) instead, run to the ZAMS. |
| `Test_m0100_feh0_spot25_ZAMS`      | A 0.1$M_\odot$ base model with the starspot filling fraction set to 0.25, run to the ZAMS; `LSDEPTH = .TRUE.`, `SPOTF = 0.25`, and `SPOTX = 0.85`. |
| `Test_m0300_feh0_allard_ZAMS`      | A 0.3$M_\odot$ base model with the Allard model atmosphere (`KTTAU = 4`) instead, run to the ZAMS. Note: the `Test_m0300_feh0_base_ZAMS` model exists, but is classified under `Evolution`. |
| `Test_m0300_feh0_spot25_ZAMS`      | A 0.3$M_\odot$ base model with the starspot filling fraction set to 0.25, run to the ZAMS; `LSDEPTH = .TRUE.`, `SPOTF = 0.25`, and `SPOTX = 0.85`. |
| `Test_m0300_feh0_scveos_ZAMS`      | A 0.3$M_\odot$ base model, but with the SCV EOS, `LOPALE06 = .FALSE.`. |
| `Test_m0300_feh0_yaleeos_ZAMS`      | A 0.3$M_\odot$ base model, but with the Yale EOS, `LOPALE06 = .FALSE.` and `LSCV = .FALSE.`. |
| `Test_m1000_feh0_spot25_ZAMS`      | A 1.0$M_\odot$ base model with the starspot filling fraction set to 0.25, run to the ZAMS; `LSDEPTH = .TRUE.`, `SPOTF = 0.25`, and `SPOTX = 0.85`. |