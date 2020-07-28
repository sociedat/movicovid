import boto3
import io
import pandas as pd
import pyarrow.parquet as pq
import s3fs client = boto3.client('s3')


s3 = s3fs.S3FileSystem()

# Extracts the date from a given key.
def get_parquet_date(key):
    index = key.index('fecha') + 6
    return key[index:index+10]

# Returns a pandas dataframe corresponding to the parquet file of the given key.
def get_parquet_file(key):
    print('Getting parquet file located at: ' + key)
    dataframe = pq.ParquetDataset('s3://movicovid/' + key, filesystem=s3) \
            .read_pandas() \
            .to_pandas()
    # Doing this as ParquetDataset.read(columns = [expected_columns]) does not
    # recognize 'columns' parameter :( and we need 'fecha' (partition column).
    parquet_file_date = get_parquet_date(key)
    date_df = [parquet_file_date for i in range(len(dataframe.index))]
    dataframe['fecha'] = date_df

    return dataframe

# Reads all the dataforgood data contained in the corresponding S3 bucket.
def get_dataforgood_data():
    # Get all parquet objects stored in raw/dataforgood.
    parquet_objects = client.list_objects_v2(
        Bucket='movicovid', Prefix='transformed/dataforgood')
    parquet_keys = [item['Key'] for item in parquet_objects['Contents'] \
                    if item['Key'].endswith('.parquet')]
    pandas_dataframes = []
    for key in parquet_keys:
        pandas_dataframes.append(get_parquet_file(key))

    return pd.concat(pandas_dataframes)

# This is the variable containing all the data from dataforgood.
dataforgood_data = get_dataforgood_data()
