
if (!"makeplotOne" %in% ls()) {
  source("../Current/Parameterized/plots.R")
}

# SWEDEN
load("sweden_1.dat")
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Sweden Runs 1-3 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Sweden/Base")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Sweden Runs 1-3 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Sweden/Base")

load("sweden_2.dat")
# Comparing delay to first introduction
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Sweden Runs 4-6 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Sweden/Base")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Sweden Runs 4-6 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Sweden/Base")

load("sweden_1_wc.dat")
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Sweden Runs 1-3 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Sweden/Worst Case")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Sweden Runs 1-3 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Sweden/Worst Case")

load("sweden_2_wc.dat")
# Comparing delay to first introduction
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Sweden Runs 4-6 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Sweden/Worst Case")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Sweden Runs 4-6 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Sweden/Worst Case")

# LATVIA
load("latvia_1.dat")
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Latvia Runs 1-3 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Latvia/Base")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Latvia Runs 1-3 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Latvia/Base")

load("latvia_2.dat")
# Comparing delay to first introduction
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Latvia Runs 4-6 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Latvia/Base")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Latvia Runs 4-6 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Latvia/Base")

load("latvia_1_wc.dat")
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Latvia Runs 1-3 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Latvia/Worst Case")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Latvia Runs 1-3 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Latvia/Worst Case")

load("latvia_2_wc.dat")
# Comparing delay to first introduction
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Latvia Runs 4-6 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Latvia/Worst Case")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Latvia Runs 4-6 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Latvia/Worst Case")


# Malta
load("malta_1.dat")
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Malta Runs 1-3 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Malta/Base")

makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
            title = "Malta Runs 1-3 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Age of Case Introduced",
            scalevals = c("Random", "Older", "Younger"))
ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Malta/Base")

load("malta_2.dat")
# Comparing delay to first introduction
makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Malta Runs 4-6 : Mean Outbreak Length",
            sub = "0.01 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Malta/Base")


makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
            title = "Malta Runs 4-6 : Mean Outbreak Length",
            sub = "0.1 Case Introduction Rate",
            scalelab = "Delay to first introduction",
            scalevals = c("12 mo", "24 mo", "36 mo"))
ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Malta/Base")

# load("malta_1_wc.dat")
# makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
#             title = "Malta Runs 1-3 : Mean Outbreak Length",
#             sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
#             scalelab = "Age of Case Introduced",
#             scalevals = c("Random", "Older", "Younger"))
# ggsave(filename = "combo_1.jpg", plot = graph, path = "./output/Malta/Worst Case")
#
# makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length",
#             title = "Malta Runs 1-3 : Mean Outbreak Length",
#             sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
#             scalelab = "Age of Case Introduced",
#             scalevals = c("Random", "Older", "Younger"))
# ggsave(filename = "combo_2.jpg", plot = graph, path = "./output/Malta/Worst Case")
#
# load("malta_2_wc.dat")
# # Comparing delay to first introduction
# makeplotOne(0.01, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
#             title = "Malta Runs 4-6 : Mean Outbreak Length",
#             sub = "0.01 Case Introduction Rate (Higher Seronegativity)",
#             scalelab = "Delay to first introduction",
#             scalevals = c("12 mo", "24 mo", "36 mo"))
# ggsave(filename = "combo_3.jpg", plot = graph, path = "./output/Malta/Worst Case")
#
# makeplotOne(0.1, xlab = "MMR Vaccination Rate", ylab = "Outbreak Length (days)",
#             title = "Malta Runs 4-6 : Mean Outbreak Length",
#             sub = "0.1 Case Introduction Rate (Higher Seronegativity)",
#             scalelab = "Delay to first introduction",
#             scalevals = c("12 mo", "24 mo", "36 mo"))
# ggsave(filename = "combo_4.jpg", plot = graph, path = "./output/Malta/Worst Case")
