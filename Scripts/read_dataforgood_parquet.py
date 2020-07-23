import boto3
import io
import pandas as pd
import pyarrow.parquet as pq
import s3fs


client = boto3.client('s3')
s3 = s3fs.S3FileSystem()

# Returns the parquet file corresponding to the given key as a pandas dataframe.
def get_parquet_file(key):
    print(key)
    return pq.ParquetDataset('s3://movicovid/' + key, filesystem=s3).read_pandas().to_pandas()

# Get all parquet objects stored in raw/dataforgood. 
parquet_objects = client.list_objects_v2(Bucket='movicovid', Prefix='transformed/dataforgood')
parquet_keys = [item['Key'] for item in parquet_objects['Contents'] if item['Key'].endswith('.parquet')]
pandas_dataframes = []
for key in parquet_keys:
    pandas_dataframes.append(get_parquet_file(key))

# This is the variable containing all the data from dataforgood.
movicovid_data = pd.concat(pandas_dataframes)
