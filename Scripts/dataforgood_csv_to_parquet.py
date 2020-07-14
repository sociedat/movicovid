import boto3
import pandas as pd
import pyarrow as pyar
import pyarrow.parquet as pypa

from s3fs import S3FileSystem


# Declaration of constants
INPUT_OBJECT_PATH = 'raw/dataforgood/'
OUTPUT_OBJECT_PATH = 's3://movicovid/transformed/dataforgood'

BUCKET_REGION = 'us-east-2'
BUCKET_NAME = 'movicovid'

PARTITION_COLUMNS = ['fecha']
S3_FILE_SYSTEM = S3FileSystem()


# Function called by AWS Lambda
def lambda_handler(event, context):
    for record in event['Records']:
        input_file_name = record['s3']['object']['key'].replace(
            INPUT_OBJECT_PATH,
            '')
        output_file_name = input_file_name.replace('.csv', '.parquet')

        # Getting data from the bucket
        s3 = boto3.client('s3', region_name=BUCKET_REGION)
        print("Reading data from the following path:")
        print(INPUT_OBJECT_PATH + input_file_name)
        bucket_object = s3.get_object(
            Bucket=BUCKET_NAME,
            Key=INPUT_OBJECT_PATH + input_file_name)
        input_body = pd.read_csv(bucket_object['Body'])
        print("Creating parquet file in the following path: ")
        print(OUTPUT_OBJECT_PATH )
        # Transforming to parquet
        data_table = pyar.Table.from_pandas(input_body)
        pypa.write_to_dataset(table=data_table,
                              root_path=OUTPUT_OBJECT_PATH,
                              partition_cols=PARTITION_COLUMNS,
                              filesystem=S3_FILE_SYSTEM)

        print("Transformation from CSV to parquet finished successfully.")
        print("New parquet file created in " +
              OUTPUT_OBJECT_PATH +
              output_file_name)

