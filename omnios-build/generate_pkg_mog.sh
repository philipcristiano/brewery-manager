/usr/bin/sh

cat > pkg.mog << EOF
set name=pkg.fmri value=${IPS_FMRI}
set name=pkg.description value=${IPS_DESCRIPTION}
set name=pkg.summary value=${IPS_SUMMARAY}
set name=variant.arch value=`uname -p`
EOF
