# Bellabeat Case Study: 
**Marketing Strategy Recommendations for Bellabeat's Smart Health Technology for Women** 

Presented by - Darryl Barnes 
# Introduction 
This case study analyzes data collected in 2016, which tracked the daily activities of 30 Fitbit users. The goal of the study is to uncover trends that could help Bellabeat refine its marketing strategy for their Leaf product. The deliverable is a high-level marketing strategy based on these insights. 

It is important to note that the dataset has limitations. The data collected in 2016, while relevant at the time, is outdated, and I will treat it as if it is still applicable to that period. This will help in constructing a framework for marketing strategies that would have been relevant in 2016. Additionally, the dataset lacks demographic details, such as gender, which poses a challenge, especially since Bellabeat’s focus is on providing women with access to healthy habits and lifestyle tools. As a result, this information alone cannot fully inform business decisions for the Leaf product. 

To address this limitation, I supplemented the dataset with historical data from the CDC’s 2015-2016 National Health and Nutrition Examination Survey (NHANES). This external data will serve to validate the trends identified within the Fitbit dataset, but it will not be used to directly influence the marketing strategy for Leaf. This validation helps in confirming that the findings align with broader trends in health and activity within the targeted demographic (women), providing additional context for the strategy. 
# Business task: 
The objective of this analysis is to examine Fitbit device usage to derive actionable insights for Bellabeat’s *Leaf* wellness tracker. Based on the findings, the goal is to provide data-driven marketing strategies aimed at increasing consumer sales. 
# Data Preparation: 
For this analysis, I used a public dataset created by Mobius, which contains Fitbit fitness tracker data. This dataset was generated from responses to a survey distributed via Amazon Mechanical Turk between December 3, 2016, and December 5, 2016. In total, 30 Fitbit users volunteered to have various aspects of their health and daily lives tracked. 

The data is organized in multiple datasets, all in long format, and was made publicly available through Kaggle. However, it’s important to note that the dataset has limitations: it is dated, and it only includes information on activity levels, with no demographic details such as age or gender of the participants. 

To ensure the analysis remained relevant and fruitful, I supplemented the Fitbit data with information from the National Health and Nutrition Examination Survey (NHANES) via the CDC’s website. I specifically used data from the 2015-2016 survey to provide context and comparison, particularly regarding physical activity levels for women during that time. This additional data helps to address the gap in demographic information within the Fitbit dataset. 

Fitbit dataset (Kaggle): [Link](https://www.kaggle.com/datasets/arashnic/fitbit) NHANES data (CDC): [Link](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015) 
# Cleaning Processing and Transforming: 
### **Noteworthy Code Chunks**  
## **Filtering NHANES data by gender.** 
nhanes\_combined <- demo **%>%![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.001.png)**

`    `**inner\_join**(sleep\_nhanes, by = "SEQN") **%>%**     **inner\_join**(paq, by = "SEQN")

nhanes\_female <- nhanes\_combined **%>%**     **filter**(RIAGENDR **==** 2)  *#2 Female*
## **Grouping NHANES data by age group** 
nhanes\_female\_active\_clean <- nhanes\_female\_active **%>% ![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.002.png)**  **mutate**(age\_group = **case\_when**(

`    `age **<** 18 **~** "<18",

`    `age **>=** 18 **&** age **<=** 25 **~** "18-25",

`    `age **>** 25 **&** age **<=** 35 **~** "26-35",

`    `age **>** 35 **&** age **<=** 45 **~** "36-45",

`    `age **>** 45 **&** age **<=** 55 **~** "46-55",

`    `age **>** 55 **&** age **<=** 65 **~** "56-65",

`    `age **>** 65 **~** "66+"

`  `))
## **Created sleep score with FitBit data** 
sleep\_data\_3 <- sleep\_data\_2 **%>%![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.003.png)**

`  `**group\_by**(id, log\_id) **%>%**

`  `**summarise**(

`    `total\_minutes = **n**(),

`    `asleep\_minutes = **sum**(value**==**1),

`    `restless\_minutes = **sum**(value**==**2),

`    `sleep\_score = asleep\_minutes **/** total\_minutes,     .groups = "drop")
## **Created unique value for distinct participants with FitBit data** 
participant\_temp <- sleep\_data\_3 **%>%** ![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.004.png)  **distinct**(id) **%>%** 

`  `**mutate**(participant = **row\_number**())

sleep\_data\_5 <- sleep\_data\_3 **%>%** 

`  `**left\_join**(participant\_temp, by = "id") **%>%**

`  `**group\_by**(participant) **%>%**

`  `**mutate**(sleep\_session = **row\_number**()) **%>%** 

`  `**select**(participant, sleep\_session, **everything**(), **-**id, **-**log\_id)

avg\_sleep\_data\_fb <- sleep\_data\_5 **%>%**

`  `**group\_by**(participant) **%>%**

`  `**summarize**(avg\_sleep\_score = **mean**(sleep\_score, na.rm = TRUE))
## **Aggregate data/collect averages per participant with FitBit sleep data** 
sleep\_data\_5 <- sleep\_data\_5 **%>%![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.005.png)**

`  `**select**(participant, sleep\_session, total\_minutes, asleep\_minutes, restless\_minutes, sleep\_score)

FB\_sleep\_data\_clean <- sleep\_data\_5 **%>%**

`  `**group\_by**(participant) **%>%**

`  `**summarise**(

`    `avg\_asleep\_hours = **round**(**mean**(asleep\_minutes, na.rm = TRUE)) **/** 60,

`    `avg\_restless\_minutes = **round**(**mean**(restless\_minutes, na.rm = TRUE)),

`    `avg\_total\_sleep\_hours = **round**(**mean**(total\_minutes, na.rm = TRUE)) **/** 60,     avg\_sleep\_score = **mean**(sleep\_score, na.rm = TRUE)

`  `)
## **Aggregate data/collect averages per participant with FitBit physical activity data** 
daily\_activity\_4 <- daily\_activity\_3 **%>%![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.006.png)**

`    `**group\_by**(participant) **%>%**

`    `**summarise**(

`        `avg\_vigorous\_activity\_min = **round**(**mean**(very\_active\_minutes, na.rm = TRUE)),         avg\_moderate\_activity\_min = **round**(**mean**(fairly\_active\_minutes, na.rm = TRUE)),         avg\_light\_activity\_min = **round**(**mean**(lightly\_active\_minutes, na.rm = TRUE)),         avg\_sedentary\_activity\_min = **round**(**mean**(sedentary\_minutes, na.rm = TRUE)),

`        `avg\_calories\_burned = **round**(**mean**(calories, na.rm = TRUE))

`    `)

fb\_pa\_data\_clean <- daily\_activity\_4 
# Insights and Findings with Visualization** 
### Daily Activity Patterns (FitBit)  
- Participants averaged 14–18 minutes of moderate to vigorous physical activity per day respectively. 
- Light activity, such as casual walking, accounted for 174 minutes per day on average. 
- 98% of individuals who engaged in over 20 minutes of vigorous activity burned more than 2,000 calories daily 

  It's important to note that device usage likely understates true activity levels for users in physically demanding jobs (e.g., retail, food service), as not all movement may be tracked during work shifts if required to remove the FitBit. 

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.007.png)

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.008.png)

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.009.png)
### Physical Activity Trends Across Ages (NHANES - CDC)** 
- Women in age groups less than 18 and over 55 reported the lowest activity levels 
- Women aged 18–35 displayed the highest levels of physical activity overall but generally participates in moderate activity more than vigorous 

These insights align with broader market research: young to middle-aged women not only engage more with health and wellness apps, they also drive spending in these categories. 

Early brand loyalty is crucial. Capturing women between 16–30 can create long-term customer value as they move into later life stages. 

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.010.jpeg)
### Sleep Behavior Insights (FitBit/NHANES-CDC) 
Note: Fitbit sleep data was incomplete; not all participants wore their trackers overnight. 

- The data suggested that the participants had wide variations in sleep patterns. 
- Although total sleep hours varied, most participants slept throughout the night. 
- Only 3 participants had sleep scores that were <0.75. 

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.011.jpeg)

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.012.jpeg)

Note: NHANES - CDC 

- Across all ages, women reported averaging 7+ hours of sleep per night 
- Women aged 55–65 showed signs of less effective sleep (lower quality, more interruptions). 
- The findings are negligent since 7 hours of sleep is considered good quality. 

Both datasets have caveats — Fitbit's reliance on usage habits and NHANES’ dependence on self-reporting — but together they point to \*\*aging women’s increasing vulnerability\*\* to sleep issues. 

![](Aspose.Words.3e5557b7-6bf1-4b6f-85c8-8d6b4faf54be.013.png)
# Recommendations 
# Market Fit  
Bellabeat currently appeals to health-conscious women. These marketing campaigns will enable strategic segmentation, allowing for messaging that appropriately resonates with different age groups. The goal is to target younger customers to build brand vitality and loyalty, while also reaching older generations to tap into underserved market opportunities. 
#### *Focus Marketing on Women Aged 16–30* 
- Launch campaigns that encourage young women to "own" their health journey early. 
- Messaging should focus on building a strong foundation for long-term wellness. 
- Position Bellabeat a part of their lifestyle tool rather than just a fitness tracker. 

By targeting this group now, we can create lifetime customers while nurturing brand advocates within their peer networks. 
##### *The "Modern Woman" Campaign* 
Bellabeat’s Leaf and app suite are essential tools for the modern, tech-savvy, health conscious woman. 

- Suggested slogan: “Own. Your. Health.” 
- Major social media campaigns using influencer marketing 
- Free trial incentive: One month of premium app insights upon signup. 
- Real-time feedback:\*Daily breakdowns of sedentary vs. active time, calories burned, and sleep quality. 

This would directly address user expectations for instant, personalized data and seamless integration into their lifestyle. 
##### *The “Never Too Late” Campaign*  
For women over 50, the messaging must emphasize ease of use, empowerment, and health preservation rather than athletic performance. 

- Highlight success stories and testimonials sling with surfacing key data points for this age group. 
- Focus on small, meaningful improvements rather than intimidating fitness goals. 
- Reinforce the idea that Bellabeat is a partner in their longevity and quality of life. 
  # Final Thoughts 
  Activity and sleep trends reveal that women aged 16–30 are highly engaged with wellness, making them ideal targets for early brand loyalty. Meanwhile, women over 50 represent an overlooked but valuable market. Bellabeat should focus marketing on young women with lifestyle-driven messaging, while also launching campaigns tailored to older women that emphasize ease, empowerment, and long-term health. 
