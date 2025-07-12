# 📸 Instagram Influencer Engagement Analysis

This project was conducted for the *Digital Marketing Intelligence* course at the University of Groningen. It explores how various content features (textual and visual) influence engagement on Instagram posts, using posts from selected Dutch and international influencers to inform the launch of a perfume brand.

---

## 🧠 Objective

To determine which influencers and content strategies (e.g., emoji use, caption length, color warmth, sentiment) lead to higher engagement rates (likes and comments per follower).

---

## 👤 Influencers Analyzed

| Influencer        | Niche        | Followers (approx) |
|-------------------|--------------|---------------------|
| @juttaleerdam      | Athlete       | 5M                  |
| @negin_mirsalehi   | Fashion       | 7M                  |
| @jeremyfragrance   | Fragrance     | 3.5M                |
| @kalvijn           | Youtuber      | 1M                  |
| @enzoknol          | Vlogger       | 2M                  |

---

## 🔍 Methods Used

### 🗂️ Data Collection
- Instagram metadata (likes, comments, caption text, etc.)
- Text cleaning and feature engineering
- Image feature extraction (e.g., warmth, saturation, object count)

### 📊 Analysis Techniques
- Exploratory Data Analysis (EDA)
- Sentiment analysis using **VADER**, **LIWC**, and **NRC** lexicons
- **Topic modeling** with LDA
- **Multiple regression models** (linear, interaction, quadratic)
- Feature importance evaluation

---

## 🧪 Key Results

- **Textual features** like emoji count, word count, positive tone, and hashtags significantly affect engagement.
- **Image warmth** shows a nonlinear relationship with likes (inverted-U).
- **Topic alignment** (e.g., lifestyle, emotional) varies across influencers.
- The best-performing regression model for likes was the **quadratic model**, achieving `R² = 0.65`.

---

## 📁 File Structure

| File | Description |
|------|-------------|
| `DMI_Assignment_Influencer_Report.pdf` | Final report with visuals, models, and recommendations  |
| `influencer_engagement_modeling.R`     | R script with EDA, modeling, and visualizations         |
| Several csv files                      | Instagram post-level data (text, image metrics, etc.)   |

---

## 💡 Managerial Recommendations

- Use emojis, hashtags, and questions to increase caption engagement.
- Choose influencers based on **goal**: e.g., Enzoknol for engagement, Negin for reach.
- Optimize image composition: 2–5 objects, warmth ≈ 0.75, saturation ≈ 0.35–0.4.

---

## 📬 Contact

- **Erlangga H. Roesdyoko**
- MSc Marketing Analytics & Data Science
- University of Groningen

---

## 📄 License

This project is for educational and academic purposes only.
