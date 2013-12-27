#! /usr/bin/env python3
# Created: 2013-02 - Olle K

#ver 0.1 - \
    # The super-basic. Can check how many unread email. Their subject, from and date
#ver 0.1.1 - \
    # Added support for non-ascii chars

#ver 0.2 @ 2013-04-06 - \
    # Ported script from python2.7 to python3.2
#ver 0.2.1 @ 2013-04-20 - \
    # Fixed bug that treated 0 mails as 1 mail


import socket # For exception handling
import imaplib # For all the IMAP functions
from getpass import getpass # Works like raw_input but does not echo
from email.parser import HeaderParser # To parse the email headers
from email.header import decode_header # To display encoded text
from sys import argv, exit

def IMAP_fetch_unread(MAIL):

    data = MAIL.search(None, 'UNSEEN')
    """ 
    MAIL.search() will return ('OK', []),
    where the list contains all email matching the search query
    - Example: ('OK', ['379 380 387'])
      ^ this means email 379, 380 and 387 in the inbox are unread
    """

    data = str(data[1][0], encoding="utf-8")
    if len(data) > 0: data = data.split(" ")
    """ Explanation for beginners:
    The initial [1] means we go into the 2nd element (0 is the first),
     data[1] would be ['379 380 387']
    The [0] means we jump to the first element of the list,
     since the list only have 1 element (the whole string)
     data[1][0] would be '379 380 387'
    Split(" ") splits all spaces in a string and returns a list
     data[1][0].split(" ") would return ['379', '380', '387']
    The reason I only split when len(data) is bigger than 0
     is that len(data) = 0 but len(data.split(" ")) is 1
    """

    print("\nYou have {} unread email".format(len(data)))
    return data

def IMAP_print_headers(MAIL, data):
    if len(data) <= 0: return
    
    print("Making list of email ..", end=" ")
    mail_list = []
    for email_num in data:
        header = MAIL.fetch(email_num, '(BODY.PEEK[HEADER])')
        parsed_header = HeaderParser().parsestr(str(header[1][0][1], encoding="utf-8"))
        mail_list.append(parsed_header)
    print("done")

    """
    MAIL.fetch() will return a specific part '(BODY.PEEK[HEADER])' of email_num
     BODY.PEEK means we wont mark the email as read
     [HEADER] mean we will only fetch the header
     the [1][0][1] is similar to MAIL.search() above, we remove boring information
    HeaderParser will parse the header and make it into a nice dictionary
    """
    
    for num in range(len(mail_list)):

        fetched_subject = decode_header(mail_list[num]["Subject"])
        subject_list = []
        for text, encode in fetched_subject:
            if encode is not None: subject = text.decode(encode)
            else: subject = text
            subject_list.append(subject)
        subject = " ".join(subject_list)

        from_person = decode_header(mail_list[num]["From"])
        from_list = []
        for person, encode in from_person:
            if encode is not None: person = person.decode(encode)
            from_list.append(person)
        from_person = " ".join(from_list)
        
        print("Mail {}: {}".format(num+1, subject))
        print("  From: {}".format(from_person))
        print("  Date: {}".format(mail_list[num]["Date"]), end="\n\n")

def IMAP_main(account, URL):

    print("Account: {} @ {}".format(account, URL))
    try: PASSWD = getpass()
    except: print(); exit()
    
    try:
        MAIL = imaplib.IMAP4_SSL(URL, 993) # Open SSL Socket
        MAIL.login(account, PASSWD) # Login
    except socket.gaierror as err:
        print("Error: %s" % err.strerror)
        exit()
    except MAIL.error as err:
        print(err)
        exit()

    MAIL.select() # No arguments == INBOX
    
    headers = IMAP_fetch_unread(MAIL)
    IMAP_print_headers(MAIL, headers)

    MAIL.close() # Close mailbox (from select)
    MAIL.logout() # Logout
    print()

if __name__ == '__main__':

    if len(argv) == 2:
        IMAP_main(argv[1], "imap.gmail.com")

    elif len(argv) == 3:
        IMAP_main(argv[1], argv[2])

    else:
        print("Usage: %s <account> <URL>")

# LOADS of info can be found here: ftp://ftp.rfc-editor.org/in-notes/rfc3501.txt
""" TAIL INFO:
Name: Check my email
Language: Python3.2
State: Done

This application checks your email (only checked with gmail)
and prints out some info


Example: ./check_my_email.py foo.bar
"""
