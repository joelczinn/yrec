# Solar models

| Model Name     | Description    |
| ------------ | -------------- |
| `Test_solar_base`      | A solar model taken to be the "base case". No diffusion, no rotation, and with a gray atmosphere. |
| `Test_solar_dif`      | The base case, with diffusion turned on: `LDIFY`, `LDIFZ`, and `LDIFLI` set to `.TRUE.`. |
| `Test_solar_dif_rot`      | The base case, with diffusion turned on, and rotation turned on: `LROT`, `LNEW0`, `LWNEW`, `LDISK` set to `.TRUE.`. |
| `Test_solar_dif_rot_fast`      | The base case, with diffusion turned on, and rotation turned on. `TDISK` and `FC` reduced relative to the `_rot` case. |
| `Test_solar_dif_rot_solid`      | The base case, with diffusion turned on, and rotation turned on. Solid body rotation `LSOLID` enforced, and `FC` increased relative to `_rot`. |
| `Test_solar_allard`      | The base case, but with an Allard model atmosphere (`KTTAU = 4`). |
| `Test_solar_kurucz`      | The base case, but with a Kurucz model atmosphere (`KTTAU = 3`). |
| `Test_solar_SF3`      | The base case, but with custom cross sections from Solar Fusion III set (`S0_*`). |
| `Test_solar_yaleeos`      | The base case, but with the Yale EOS, `LOPALE06 = .FALSE.` and `LSCV = .FALSE.`. |