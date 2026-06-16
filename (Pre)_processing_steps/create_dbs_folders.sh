base="/home/rafaelp/META-BRAIN/open-DBS"

for i in $(seq -w 1 14); do
  subj="sub-DBS$i"
  mkdir -p "$base/$subj/diffusion_output"/{ses-preop,ses-postop01m,ses-postop03m,ses-postop06m,ses-postop12m}
done
