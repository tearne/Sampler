PROJ_DIR=~/Sampler
TARGET_DIR=$PROJ_DIR/sampler-examples/deploy/lib

find $PROJ_DIR/lib_managed -name '*.jar' -exec cp '{}' $TARGET_DIR \;
cp $PROJ_DIR/sampler-core/target/scala-2.10/sampler-core_*.jar $TARGET_DIR
cp $PROJ_DIR/sampler-cluster/target/scala-2.10/sampler-cluster_*.jar $TARGET_DIR
cp $PROJ_DIR/sampler-examples/target/scala-2.10/sampler-examples_*.jar $TARGET_DIR
