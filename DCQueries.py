# -*- coding: utf-8 -*-
#"""
#Spyder Editor

#This is a temporary script file.
K0 = "Energy Efficiency"
K1 = ["Data models","Model development and analysis"]
K2 = ["Decision Making", "Decision Support Systems"]
K3 = ["Machine Learning", "Data Mining"]
K4 = ["Big Data applications","MapReduce-based systems"]
queries= []
keywords = [K1,K2,K3,K4]
n = 0
q2 = ""
len(keywords)

#Energy efficiency and separated levels
for slist in keywords:
    q1= "(\'"+K0+"\' AND ("
    for j in slist:
        q2= q2+"'"+j+"'"
        if j != slist[-1]:
            q2 = q2+ " OR "
        else:
            q2 = q2 + "))"
            q2=q1+q2
            queries.append(q2)
            q2=""

print (queries[0]+"\r\n 1")
print (queries[1]+"\r\n 2")
print (queries[2]+"\r\n 3")
print (queries[3]+"\r\n 4")

#Energy efficiency and combined levels

q2= "(\'"+K0+"\' AND ("
for slist in keywords:    
    for j in slist:
        q2= q2+"'"+j+"'"
        if j != slist[-1]:
            q2 = q2+ " OR "
        else:
            q2 = q2 + ")"
    if(slist != keywords[-1]):
        q2 = q2+ " AND ( "
    else:
        q2 = q2+ " ) "  

print (q2)
