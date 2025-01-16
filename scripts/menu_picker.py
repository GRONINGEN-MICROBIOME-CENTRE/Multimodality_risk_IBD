import pandas as pd
df_loc = pd.read_csv('/groups/umcg-lifelines/prm02/projects/ov23_0813/municipalcodes_restructure_data_20240530/sec_l_cbs_results_restructured.csv')
#df_people = pd.read_csv('LLD_env.csv')
df_people = pd.read_csv('DAG3env.csv')
df_loc['neighbourhoodcode_enddate'][df_loc['neighbourhoodcode_enddate'] == ' '] = "2024-05"
df_loc['neighbourhoodcode_enddate']= pd.to_datetime(df_loc['neighbourhoodcode_enddate'])
df_loc['neighbourhoodcode_startdate']= pd.to_datetime(df_loc['neighbourhoodcode_startdate'])

current_municipality_values_12 = []
current_municipality_values_14 = []
current_municipality_values_15 = []
current_municipality_values_16 = []

current_neighbour_values_12 = []
current_neighbour_values_14 = []
current_neighbour_values_15 = []
current_neighbour_values_16 = []

current_district_values_12 = []
current_district_values_14 = []
current_district_values_15 = []
current_district_values_16 = []

for people in df_people['project_pseudo_id']:
	current_iter_df = df_loc[df_loc['project_pseudo_id'] == people]
	current_iter_date = df_people[df_people['project_pseudo_id'] == people]['date'].values[0]
	current_year = df_people[df_people['project_pseudo_id'] == people ]['date'].values[0][0:4]
	current_iter_menu = current_iter_df[(current_iter_df['neighbourhoodcode_enddate'] > df_people[df_people['project_pseudo_id'] == people]['date'].values[0]) & (current_iter_df['neighbourhoodcode_startdate'] < df_people[df_people['project_pseudo_id'] == people]['date'].values[0])]['municipalcode_{}'.format(current_year)].values
	current_iter_neigh = current_iter_df[(current_iter_df['neighbourhoodcode_enddate'] > df_people[df_people['project_pseudo_id'] == people]['date'].values[0]) & (current_iter_df['neighbourhoodcode_startdate'] < df_people[df_people['project_pseudo_id'] == people]['date'].values[0])]['neighbourhoodcode_{}'.format(current_year)].values
	current_iter_dist = current_iter_df[(current_iter_df['neighbourhoodcode_enddate'] > df_people[df_people['project_pseudo_id'] == people]['date'].values[0]) & (current_iter_df['neighbourhoodcode_startdate'] < df_people[df_people['project_pseudo_id'] == people]['date'].values[0])]['districtcode_{}'.format(current_year)].values
	if current_year == '2012':

		current_municipality_values_14.append('nan')
		current_neighbour_values_14.append('nan')
		current_district_values_14.append('nan')

		current_municipality_values_15.append('nan')
		current_neighbour_values_15.append('nan')
		current_district_values_15.append('nan')

		current_municipality_values_16.append('nan')
		current_neighbour_values_16.append('nan')
		current_district_values_16.append('nan')

		if current_iter_menu:
			current_municipality_values_12.append(current_iter_menu[0])
			current_neighbour_values_12.append(current_iter_neigh[0])
			current_district_values_12.append(current_iter_dist[0])
		else:
			current_year = str(int(current_year) -1)
#			current_iter_menu = current_iter_df[(current_iter_df['neighbourhoodcode_enddate'] >df_people[df_people['project_pseudo_id'] == people]['date'].values[0]) & (current_iter_df['neighbourhoodcode_startdate'] <df_people[df_people['project_pseudo_id'] == people]['date'].values[0])]['municipalcode_{}'.format(current_year)].values
			if current_iter_menu:
				current_municipality_values_12.append(current_iter_menu[0])
				current_neighbour_values_12.append(current_iter_neigh[0])
				current_district_values_12.append(current_iter_dist[0])
			else:
				current_municipality_values_12.append('missing')
				current_neighbour_values_12.append('missing')
				current_district_values_12.append('missing')
	elif current_year == '2014':
		current_municipality_values_12.append('nan')
		current_neighbour_values_12.append('nan')
		current_district_values_12.append('nan')

		current_municipality_values_15.append('nan')
		current_neighbour_values_15.append('nan')
		current_district_values_15.append('nan')

		current_municipality_values_16.append('nan')
		current_neighbour_values_16.append('nan')
		current_district_values_16.append('nan')


		if current_iter_menu:
			current_municipality_values_14.append(current_iter_menu[0])
			current_neighbour_values_14.append(current_iter_neigh[0])
			current_district_values_14.append(current_iter_dist[0])
		else:
			current_municipality_values_14.append('missing')
			current_neighbour_values_14.append('missing')
			current_district_values_14.append('missing')


	elif current_year == '2015':

		current_municipality_values_12.append('nan')
		current_neighbour_values_12.append('nan')
		current_district_values_12.append('nan')

		current_municipality_values_14.append('nan')
		current_neighbour_values_14.append('nan')
		current_district_values_14.append('nan')

		current_municipality_values_16.append('nan')
		current_neighbour_values_16.append('nan')
		current_district_values_16.append('nan')

		if current_iter_menu:
			current_municipality_values_15.append(current_iter_menu[0])
			current_neighbour_values_15.append(current_iter_neigh[0])
			current_district_values_15.append(current_iter_dist[0])
		else:
			current_year = str(int(current_year) -1)
#			current_iter_menu = current_iter_df[(current_iter_df['neighbourhoodcode_enddate'] >df_people[df_people['project_pseudo_id'] == people]['date'].values[0]) & (current_iter_df['neighbourhoodcode_startdate'] <df_people[df_people['p$
			if current_iter_menu:
				current_municipality_values_15.append(current_iter_menu[0])
				current_neighbour_values_15.append(current_iter_neigh[0])
				current_district_values_15.append(current_iter_dist[0])
			else:
				current_municipality_values_15.append('missing')
				current_neighbour_values_15.append('missing')
				current_district_values_15.append('missing')


	elif current_year == '2016':
		current_municipality_values_12.append('nan')
		current_neighbour_values_12.append('nan')
		current_district_values_12.append('nan')

		current_municipality_values_15.append('nan')
		current_neighbour_values_15.append('nan')
		current_district_values_15.append('nan')

		current_municipality_values_14.append('nan')
		current_neighbour_values_14.append('nan')
		current_district_values_14.append('nan')


		if current_iter_menu:
			current_municipality_values_16.append(current_iter_menu[0])
			current_neighbour_values_16.append(current_iter_neigh[0])
			current_district_values_16.append(current_iter_dist[0])
		else:
			current_municipality_values_16.append('missing')
			current_neighbour_values_16.append('missing')
			current_district_values_16.append('missing')



	else:
		current_municipality_values_12.append('Missing')
		current_neighbour_values_12.append('Missing')
		current_district_values_12.append('Missing')
		
		current_municipality_values_14.append('Missing')
		current_neighbour_values_14.append('Missing')
		current_district_values_14.append('Missing')

		current_municipality_values_15.append('Missing')
		current_neighbour_values_15.append('Missing')
		current_district_values_15.append('Missing')

		current_municipality_values_16.append('Missing')
		current_neighbour_values_16.append('Missing')
		current_district_values_16.append('Missing')

#print(current_municipality_values)

df_people['municipality_at_sampling_2012'] = current_municipality_values_12
df_people['municipality_at_sampling_2014'] = current_municipality_values_14
df_people['municipality_at_sampling_2015'] = current_municipality_values_15
df_people['municipality_at_sampling_2016'] = current_municipality_values_16

df_people['neighbourhood_at_sampling_2012'] = current_neighbour_values_12
df_people['neighbourhood_at_sampling_2014'] = current_neighbour_values_14
df_people['neighbourhood_at_sampling_2015'] = current_neighbour_values_15
df_people['neighbourhood_at_sampling_2016'] = current_neighbour_values_16

df_people['district_at_sampling_2012'] = current_district_values_12
df_people['district_at_sampling_2014'] = current_district_values_14
df_people['district_at_sampling_2015'] = current_district_values_15
df_people['district_at_sampling_2016'] = current_district_values_16


df_people.to_csv('DAG3env_with_menu.csv', index=False)
