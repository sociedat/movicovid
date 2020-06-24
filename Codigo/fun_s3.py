#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import boto3



def abre_file_como_df(bucket_name, file_to_read):
     "Esta funcion abre un archivo del bucket en un dataframe"

     ses = boto3.session.Session(profile_name='default', region_name='us-east-2')
     client = boto3.client('s3', region_name='us-east-2')

     #create a file object using the bucket and object key. 
     fileobj = client.get_object(Bucket=bucket_name, Key=file_to_read) 

     df = pd.read_csv(fileobj['Body'], sep=",")

     return df
