import collections
import datetime
import time
import xml.etree.ElementTree

import requests


def esearch_query(payload, retmax = 10000, sleep=0.34):
    """
    Return identifiers using the ESearch E-utility.
    """
    url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi'
    payload['rettype'] = 'xml'
    payload['retmax'] = retmax
    payload['retstart'] = 0
    ids = list()
    count = 1
    while payload['retstart'] < count:
        response = requests.get(url, params=payload)
        tree = xml.etree.ElementTree.fromstring(response.text)
        count = int(tree.findtext('Count'))
        ids += [id_.text for id_ in tree.findall('IdList/Id')]
        payload['retstart'] += retmax
        print('esearch {:.3%} complete'.format(payload['retstart'] / count), end='\r')
        time.sleep(sleep)
    return ids


def pubmed_esummary(ids, write_file, retmax=100, retmin=20, sleep=0.34, error_sleep=10):
    """Submit an ESummary query for PubMed records and write results as xml to write_file."""
    
    # Base URL for PubMed's esummary eutlity
    url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi'
        
    # Set up progress stats
    n_total = len(ids)
    n_complete = 0
    start_time = time.perf_counter()
    time_last = time.perf_counter()
    successive_errors = 0
    
    # Write first line of XML
    write_file.write('<eSummaryResult>\n')

    # Set up queue
    idq = collections.deque()
    for i in range(0, len(ids), retmax):
        idq.append(ids[i:i+retmax])

    # Query until the queue is empty
    while idq:
        time.sleep(sleep)
        id_subset = idq.popleft()
        id_subset_len = len(id_subset)
        
        # Perform eutilities API request
        id_string = ','.join(map(str, id_subset))
        payload = {'db': 'pubmed', 'id': id_string, 'rettype': 'xml'}
        time_this = time.perf_counter()
        wait = time_this - time_last
        time_last = time_this
        try:
            response = requests.get(url, params=payload)
            tree = xml.etree.ElementTree.fromstring(response.text)
            successive_errors = 0
        except Exception as e:
            successive_errors += 1
            print('{} successive error: {} IDs [{} ... {}] threw {}'.format(
                    successive_errors, id_subset_len, id_subset[0], id_subset[-1], e))
            if id_subset_len >= retmin * 2:
                mid = len(id_subset) // 2
                idq.appendleft(id_subset[:mid])
                idq.appendleft(id_subset[:mid])
            else:
                idq.appendleft(id_subset)
            time.sleep(error_sleep * successive_errors)
            continue

        # Write XML to file
        for docsum in tree.getchildren():
            xml_str = xml.etree.ElementTree.tostring(docsum, encoding='unicode')
            write_file.write(xml_str)
        
        # Report progress
        n_complete += id_subset_len
        time_per_id = (time.perf_counter() - start_time) / n_complete
        remaining = (n_total - n_complete) * time_per_id
        remaining = datetime.timedelta(seconds=round(remaining))
        print('{:.4%} complete; {:.3f} seconds since last API call; {} remaining'.format(
                n_complete / n_total, wait, remaining), end='\r')

    # Write final line of XML
    write_file.write('</eSummaryResult>\n')
