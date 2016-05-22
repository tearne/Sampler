PROJ_DIR=~/ENV/code/Sampler
TARGET_DIR=$PROJ_DIR/sampler-examples/deployment/payload/lib

mkdir -p $TARGET_DIR

find $PROJ_DIR/lib_managed -name '*.jar' -exec cp '{}' $TARGET_DIR \;
cp $PROJ_DIR/sampler-core/target/scala-2.11/sampler-core_*.jar $TARGET_DIR
cp $PROJ_DIR/sampler-abc/target/scala-2.11/sampler-abc*.jar $TARGET_DIR
cp $PROJ_DIR/sampler-examples/target/scala-2.11/sampler-examples_*.jar $TARGET_DIR
cp $PROJ_DIR/sampler-r/target/scala-2.11/sampler-r_*.jar $TARGET_DIR

rm $TARGET_DIR/*-javadoc.jar
rm $TARGET_DIR/*-sources.jar
