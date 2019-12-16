import os
import glob
import csv
import sys
import math
# import numpy as np

from collections import defaultdict
from datetime import datetime, timedelta
from pprint import pprint


def preprocess_nashville(input_data_folder, output_data_folder, 
        period_length=180, period_start_date=datetime(2009, 1, 1),
        period_end_date=datetime(2018, 7, 18)):

    force_filename = os.path.join(input_data_folder, 'nashville_use_of_force.csv')
    assigments_filename = os.path.join(input_data_folder, 'nashville_police_assignments.csv')
    complaints_filename = os.path.join(input_data_folder, 'nashville_complaints.csv')
    
    # Built-In Variables
    community_divisions = [x + ' Precinct Division' for x in ['South', 'West', 'East', 'North',
        'Central', 'Hermitage']] + \
        [x + ' Precinct' for x in ['Madison', 'Mid-Town Hills']]
    all_time_periods = range(int(math.floor((period_end_date - period_start_date).days / period_length)))
    time_period_starts = [timedelta(180 * x) + period_start_date for x in all_time_periods]

    # Extract employee and assingment information
    employee_dict = defaultdict(lambda: defaultdict(list))
    with open(assigments_filename, 'r') as openfile:
        reader = csv.reader(openfile, delimiter=',')

        # Skip Header
        next(reader)

        for row in reader:
            emp_id = row[0]

            # Variables to be used later.
            employee_dict[emp_id]['error'] = False
            employee_dict[emp_id]['missing'] = False

            # Initailize Complaint Counts
            employee_dict[emp_id]['complaint_count'] = 0
            employee_dict[emp_id]['civilian_complaint_count'] = 0
            employee_dict[emp_id]['complaint_count_assignment'] = 0
            employee_dict[emp_id]['civilian_complaint_count_assignment'] = 0
            
            # Demographic Variables
            employee_dict[emp_id]['gender'] = row[-2]
            if row[-2] not in ['M', 'F']:
                employee_dict[emp_id]['error'] = True
            employee_dict[emp_id]['race'] = row[-3]
            
            # Assignment Variables
            employee_dict[emp_id]['bureaus'] += [row[1]]
            employee_dict[emp_id]['divisions'] += [row[2]]
            employee_dict[emp_id]['sections'] += [row[3]]
            
            # Age Variables
            employee_dict[emp_id]['ages'] += [int(row[-1])]
            if int(row[-1]) > 100:
                employee_dict[emp_id]['error'] = True
            if 'max_age' not in employee_dict[emp_id].keys():
                employee_dict[emp_id]['max_age'] = int(row[-1])
            else:
                employee_dict[emp_id]['max_age'] = max(int(row[-1]), employee_dict[emp_id]['max_age'])

            # Assignment Duration Variables
            employee_dict[emp_id]['hire_date'] = datetime.strptime(row[-4][:-4], '%Y-%m-%d %H:%M:%S')
            employee_dict[emp_id]['start_dates'] += [datetime.strptime(row[-6][:-4], '%Y-%m-%d %H:%M:%S')]            
            end_date = row[-5][:-4]
            if end_date == '3000-01-01 00:00:00':
                # Adjust this to time data was retrieved.
                # employee_dict[emp_id]['end_dates'] += [datetime.today()]
                if len(employee_dict[emp_id]['end_dates']) == 0:
                    employee_dict[emp_id]['end_dates'] += [datetime(2019, 1, 1)]
                    employee_dict[emp_id]['error'] = True
                else:
                    employee_dict[emp_id]['end_dates'] += [employee_dict[emp_id]['end_dates'][-1]]
            else:
                employee_dict[emp_id]['end_dates'] += [datetime.strptime(row[-5][:-4], '%Y-%m-%d %H:%M:%S')]

            # Experience Variables
            employee_dict[emp_id]['experiences'] += [(employee_dict[emp_id]['start_dates'][-1] - employee_dict[emp_id]['hire_date']).days]
            employee_dict[emp_id]['max_experience'] = (datetime(2019, 1, 1) - employee_dict[emp_id]['hire_date']).days
            
    # Employee variables which require the whole employment history.
    for emp_id, item in employee_dict.items():
        start_date = item['start_dates'][0]
        end_date = item['end_dates'][-1]
        employee_dict[emp_id]['start_date'] = start_date
        employee_dict[emp_id]['end_date'] = end_date
        employee_dict[emp_id]['days_assigned'] = (end_date - start_date).days + 1

        # Defining assignment switches.
        switches = 0
        community_switches = 0
        for idx, section in enumerate(item['sections']):
            division = item['divisions'][idx]
            if idx == 0:
                current_section = section
                current_division = division
            else:
                if division != current_division:
                    if division in community_divisions and current_division in community_divisions:
                        community_switches += 1
                        employee_dict[emp_id]['community_switch_dates'] += [employee_dict[emp_id]['start_dates'][idx]]
                    switches += 1
                    employee_dict[emp_id]['switch_dates'] += [employee_dict[emp_id]['start_dates'][idx]]
                current_section = section
                current_division = division
        employee_dict[emp_id]['switches'] = switches
        employee_dict[emp_id]['community_switches'] = community_switches

        # Note: we can't truly know who is active, because 
        # end dates are not well defined.
        employee_dict[emp_id]['active'] = end_date.date() >= datetime(2019, 1, 1).date()

    # Extract complaint information
    complaint_dict = defaultdict(lambda: defaultdict(int))
    with open(complaints_filename, 'r') as openfile:
        reader = csv.reader(openfile, delimiter=',')
        original_header = next(reader)

        for row in reader:
            emp_id = row[6]
            complaint_id = row[9]
            complaint_dict[complaint_id]['emp_id'] = emp_id
            complaint_dict[complaint_id]['original_data'] = row
            employee_dict[emp_id]['first_name'] = row[4]
            employee_dict[emp_id]['last_name'] = row[3]
            employee_dict[emp_id]['full_name'] = row[4] + ' ' + row[3]
            complaint_dict[complaint_id]['gender'] = employee_dict[emp_id]['gender']
            complaint_dict[complaint_id]['race'] = employee_dict[emp_id]['race']
            date = datetime.strptime(row[2], '%B %d, %Y')
            complaint_dict[complaint_id]['date'] = date

            previous_end_date = datetime(1900, 1, 1)
            time_fields = ['bureau', 'division', 'section', 'age', 'experience']
            for date_index, end_date in enumerate(employee_dict[emp_id]['end_dates']):
                if date_index == 0 and date < end_date:
                    for field in time_fields:
                        complaint_dict[complaint_id][field] = 'Unassigned'
                    complaint_dict[complaint_id]['during_assignment'] = False
                    break
                if date <= end_date and date > previous_end_date:
                    for field in time_fields:
                        complaint_dict[complaint_id][field] = employee_dict[emp_id][field + 's'][date_index]
                    complaint_dict[complaint_id]['during_assignment'] = True
                    break
                if date_index == len(employee_dict[emp_id]['end_dates']) - 1:
                    for field in time_fields:
                        complaint_dict[complaint_id][field] = employee_dict[emp_id][field + 's'][-1]
                    complaint_dict[complaint_id]['during_assignment'] = False
                    break
                previous_end_date = end_date

            for key in ['bureau', 'division', 'section']:
                if key not in complaint_dict[complaint_id].keys():
                    complaint_dict[complaint_id][key] = ''

            if 'complaint_count' not in employee_dict[emp_id]:
                employee_dict[emp_id]['civilian_complaint_count'] = 1
                employee_dict[emp_id]['complaint_count'] = 1
                employee_dict[emp_id]['civilian_complaint_count_assignment'] = 1
                employee_dict[emp_id]['complaint_count_assignment'] = 1
                employee_dict[emp_id]['missing'] = True
            else:
                if row[-4] == 'Citizen':
                    employee_dict[emp_id]['civilian_complaint_count'] += 1
                    if complaint_dict[complaint_id]['during_assignment']:
                        employee_dict[emp_id]['civilian_complaint_count_assignment'] += 1
                employee_dict[emp_id]['complaint_count'] += 1
                if complaint_dict[complaint_id]['during_assignment']:
                    employee_dict[emp_id]['complaint_count_assignment'] += 1

    # Derived Statistics
    copy_employee_dict = employee_dict.copy()
    for key, item in copy_employee_dict.items():
        if employee_dict[key]['missing']:
            for field in ['complaint_per_day', 'civilian_complaint_per_day',
                    'civilian_complaint_per_year', 'complaint_per_year',
                    'complaint_assignment_per_day', 
                    'complaint_assignment_per_year',
                    'civilian_complaint_assignment_per_day',
                    'civilian_complaint_assignment_per_year']:
                employee_dict[key][field] = 'null'
        else:
            for metric in ['complaint_count', 'civilian_complaint_count', 
                    'complaint_count_assignment', 
                    'civilian_complaint_count_assignment', 'switches',
                    'community_switches']:
                # print(employee_dict[key][metric])
                employee_dict[key][f'{metric}_per_day'] = employee_dict[key][metric] / employee_dict[key]['days_assigned']
                employee_dict[key][f'{metric}_per_year'] = employee_dict[key][f'{metric}_per_day'] * 365
    del(copy_employee_dict)

    # Create extended complaints
    complaints_plus = os.path.join(output_data_folder, 
        'nashville_complaints_extended.csv')
    with open(complaints_plus, 'w', newline='') as outfile:
        writer = csv.writer(outfile, delimiter=',')
        header = original_header + ['age', 'gender', 'race', 'bureau', 
            'division', 'section', 'experience', 'missing', 'days_assigned', 
            'complaint_count_assignment', 
            'civilian_complaint_count_assignment',
            'complaint_assignment_per_day', 'complaint_assignment_per_year',
            'civilian_complaint_assignment_per_day',
            'civilian_complaint_assignment_per_year',
            'complaint_count', 
            'civilian_complaint_count', 'complaint_per_day', 
            'complaint_per_year', 'civilian_complaint_per_day', 
            'civlian_complaint_per_year', 'during_assignment']
        writer.writerow(header)

        for complaint_id, item in complaint_dict.items():
            emp_id = item['emp_id']
            output_row = item['original_data']
            output_row += [item['age'], item['gender'], item['race'],
                item['bureau'], item['division'], item['section'],
                item['experience']]
            output_row += [employee_dict[emp_id]['missing'],
                employee_dict[emp_id]['active'],
                employee_dict[emp_id]['days_assigned'],
                employee_dict[emp_id]['complaint_count_assignment'],
                employee_dict[emp_id]['civilian_complaint_count_assignment'],
                employee_dict[emp_id]['complaint_assignment_per_day'],
                employee_dict[emp_id]['complaint_assignment_per_year'],
                employee_dict[emp_id]['civilian_complaint_assignment_per_day'],
                employee_dict[emp_id]['civilian_complaint_assignment_per_year'],
                employee_dict[emp_id]['complaint_count'],
                employee_dict[emp_id]['civilian_complaint_count'],
                employee_dict[emp_id]['complaint_per_day'],
                employee_dict[emp_id]['complaint_per_year'],
                employee_dict[emp_id]['civilian_complaint_per_day'],
                employee_dict[emp_id]['civilian_complaint_per_year'],
                item['during_assignment']]
            writer.writerow(output_row)

    # Create cop spreadsheet
    cop_list = os.path.join(output_data_folder, 
        'nashville_cop_details.csv')

    with open(cop_list, 'w', newline='') as outfile:
        writer = csv.writer(outfile, delimiter=',')
        header = ['emp_id', 'name', 'first_name', 'last_name', 'max_age',
            'gender', 'race', 'missing',
            'start_date', 'end_date', 'hire_date', 'max_experience', 
            'days_assigned', 'switches', 'switches_per_day',
            'switches_per_year', 
            'community_switches', 'community_switches_per_day',
            'community_switches_per_year', 'complaint_count_assignment', 
            'civilian_complaint_count_assignment',
            'complaint_count_assignment_per_day', 
            'complaint_count_assignment_per_year',
            'civilian_complaint_count_assignment_per_day',
            'civilian_complaint_count_assignment_per_year',
            'complaint_count',
            'civilian_complaint_count', 'complaint_count_per_day', 
            'complaint_count_per_year',
            'civilian_complaint_count_per_day', 
            'civilian_complaint_count_per_year']
        writer.writerow(header)

        for emp_id, item in employee_dict.items():
            output_row = [emp_id]
            for field in header[1:]:
                output_row += [item[field]]
            writer.writerow(output_row)

    # Create model spreadsheet.
    model_spreadsheet = os.path.join(output_data_folder,
        'nashville_model_formatted.csv')

    time_index = 0
    gender_dict = {'F': 0, 'M': 1, ' ': 2}
    race_dict = {'A': 0, 'B': 1, 'I': 2, 'T': 3, 'W': 4, 'H': 5, ' ': 6}
    model_header = ['police_id', 'time_period', 'division', 'complaints', 
    'race', 'gender', 'age', 'experience']
    division_dict = {x: i for i, x in enumerate(community_divisions)}
    model_dict = defaultdict(lambda: defaultdict(dict))

    for emp_id, item in employee_dict.items():
        if item['missing'] or item['error']:
            continue
        start_date = item['start_dates'][0]
        end_date = item['end_dates'][-1]
        switch_time_periods = []
        for switch_date in employee_dict[emp_id]['switch_dates']:
            switch_time_periods += [int(math.floor((switch_date - period_start_date).days / period_length))]
        time_periods = range(int(math.floor((start_date - period_start_date).days / period_length)),
            int(math.floor((end_date - period_start_date).days / period_length)))

        # print(start_date, end_date, period_start_date, period_length)
        # print((end_date - period_start_date).days / period_length)
        # print(time_periods)
        for time_period in time_periods:
            if time_period > int(math.floor((period_end_date - period_start_date).days / period_length)):
                continue
            if time_period <= 0:
                continue
            time_period_start = timedelta(days=period_length * time_period) + period_start_date
            previous_idx = 0
            for idx, start_date in enumerate(item['start_dates']):
                # print(idx, start_date, time_period_start)
                if start_date > time_period_start:
                    # pprint(item['start_dates'])
                    # print(time_period, time_period_start, start_date)
                    # pprint(item['divisions'])
                    # print(previous_idx)
                    division = item['divisions'][previous_idx]
                    if division not in community_divisions:
                        break
                    model_dict[emp_id][time_period] = defaultdict(int)
                    model_dict[emp_id][time_period]['complaints'] = 0
                    model_dict[emp_id][time_period]['division'] = division_dict[division]
                    model_dict[emp_id][time_period]['age'] = item['ages'][previous_idx]
                    model_dict[emp_id][time_period]['experience'] = item['experiences'][previous_idx]
                    model_dict[emp_id][time_period]['time_period_start'] = time_period_start
                    model_dict[emp_id][time_period]['start_date'] = start_date
                    model_dict[emp_id]['gender'] = gender_dict[employee_dict[emp_id]['gender']]
                    model_dict[emp_id]['race'] = race_dict[employee_dict[emp_id]['race']]
                    if time_period in switch_time_periods:
                        model_dict[emp_id][time_period]['switch'] = True
                    else:
                        model_dict[emp_id][time_period]['switch'] = False
                    break
                previous_idx = idx

        # if emp_id == '109495':
            # pprint(model_dict[emp_id])
            # pprint(item)
            # return

    for complaint_id, item in complaint_dict.items():
        emp_id = item['emp_id']
        date = item['date']
        if employee_dict[emp_id]['missing']:
            continue
        if item['division'] not in community_divisions:
            continue
        if date < period_start_date or date > period_end_date:
            continue
        time_period = int(math.floor((date - period_start_date).days / period_length))
        if time_period not in model_dict[emp_id].keys():
            continue
        model_dict[emp_id][time_period]['complaints'] += 1

    with open(model_spreadsheet, 'w', newline='') as outfile:
        writer = csv.writer(outfile, delimiter=',')
        writer.writerow(model_header)
        for emp_id, emp_dict in model_dict.items():
            for time_period, time_dict in emp_dict.items():
                # pprint(emp_dict)
                # pprint(time_dict)
                if emp_id == [' ']:
                    continue
                if time_period in ['gender', 'race']:
                    continue
                if time_dict['switch']:
                    continue
                output_row = [emp_id, time_period, time_dict['division'],
                    time_dict['complaints'], emp_dict['race'],
                    emp_dict['gender'], time_dict['age'],
                    time_dict['experience']]
                writer.writerow(output_row)

    return


def test_analysis():

    import statsmodels.api as sm
    import pandas as pd
    pd.set_option('display.max_rows', 1000)
    import statsmodels.formula.api as smf
    data = sm.datasets.get_rdataset("dietox", "geepack").data
    # md = smf.mixedlm("Weight ~ Feed", data, groups=data["Pig"])
    # mdf = md.fit()
    # print(mdf.summary())
    # pprint(data)
    # print(data.dtypes)
    cop_data = pd.read_csv("../../data/processed/nashville/nashville_model_formatted.csv",
        dtype={"complaints": float, "experience": float, 'age': float})
    print(cop_data)
    print(cop_data.dtypes)
    cop_md = smf.mixedlm("complaints ~ time_period + age + experience", cop_data, groups=cop_data["police_id"])
    cop_mdf = cop_md.fit()
    print(cop_mdf.summary())
    return


if __name__ == '__main__':

    # Nashville
    input_dir = '../../data/original/nashville'
    output_dir = '../../data/processed/nashville'
    preprocess_nashville(input_dir, output_dir)
    # test_analysis()

    pass