__author__ = 'goran'



def writeData(path='../R/data/u.item'):
    '''
    dropping unimportant data for the data mining techniques (id, movie url, date of release...)
    and writing the important data to itemsData file, which will be used in the R scripts
    :param path:
    :return:
    '''
    out = file('../R/data/itemsData.txt', 'w')
    out.write('Movie')
    genres = 'unknown | Action | Adventure | Animation | Children | Comedy | Crime | Documentary | Drama | Fantasy | Film-Noir | Horror | Musical | Mystery | Romance | Sci-Fi | Thriller | War | Western'
    for g in genres.split(' | '):
        out.write('\t' + g)
    out.write('\n')

    lines = [line for line in open(path)]

    for line in lines:
        line = line.strip()
        if len(line) == 0:
            continue
        spl = line.split('|')
        out.write(spl[1])
        for i in xrange(len(spl) - 19, len(spl)):
            out.write('\t' + spl[i])
        out.write('\n')

writeData()


