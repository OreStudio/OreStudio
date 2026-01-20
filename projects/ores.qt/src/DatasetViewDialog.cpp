/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/DatasetViewDialog.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QSplitter>
#include <QHeaderView>
#include <QPainterPath>
#include <QGraphicsTextItem>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

DatasetViewDialog::DatasetViewDialog(ClientManager* clientManager,
                                     QWidget* parent)
    : QDialog(parent), clientManager_(clientManager) {
    setupUi();
}

DatasetViewDialog::~DatasetViewDialog() = default;

void DatasetViewDialog::setupUi() {
    setWindowTitle(tr("Dataset Details"));
    setMinimumSize(800, 600);
    resize(950, 700);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    // Create tab widget with tabs at the top
    tabWidget_ = new QTabWidget(this);

    // Add tabs
    tabWidget_->addTab(createOverviewTab(), tr("Overview"));
    tabWidget_->addTab(createProvenanceTab(), tr("Provenance"));
    tabWidget_->addTab(createMethodologyTab(), tr("Methodology"));
    tabWidget_->addTab(createLineageTab(), tr("Lineage"));

    mainLayout->addWidget(tabWidget_);
}

QWidget* DatasetViewDialog::createOverviewTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(8, 8, 8, 8);

    overviewTree_ = new QTreeWidget();
    overviewTree_->setHeaderLabels({tr("Property"), tr("Value")});
    overviewTree_->setAlternatingRowColors(true);
    overviewTree_->setRootIsDecorated(false);
    overviewTree_->header()->setStretchLastSection(true);
    overviewTree_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    overviewTree_->setStyleSheet("QTreeWidget::item { padding: 4px 0px; }");

    layout->addWidget(overviewTree_);
    return widget;
}

QWidget* DatasetViewDialog::createProvenanceTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(8, 8, 8, 8);

    provenanceTree_ = new QTreeWidget();
    provenanceTree_->setHeaderLabels({tr("Property"), tr("Value")});
    provenanceTree_->setAlternatingRowColors(true);
    provenanceTree_->setRootIsDecorated(false);
    provenanceTree_->header()->setStretchLastSection(true);
    provenanceTree_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    provenanceTree_->setStyleSheet("QTreeWidget::item { padding: 4px 0px; }");

    layout->addWidget(provenanceTree_);
    return widget;
}

QWidget* DatasetViewDialog::createMethodologyTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(8, 8, 8, 8);

    auto* splitter = new QSplitter(Qt::Vertical);

    // Top: properties tree
    methodologyTree_ = new QTreeWidget();
    methodologyTree_->setHeaderLabels({tr("Property"), tr("Value")});
    methodologyTree_->setAlternatingRowColors(true);
    methodologyTree_->setRootIsDecorated(false);
    methodologyTree_->header()->setStretchLastSection(true);
    methodologyTree_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    methodologyTree_->setStyleSheet("QTreeWidget::item { padding: 4px 0px; }");

    // Bottom: implementation details text
    stepsText_ = new QTextBrowser();
    stepsText_->setPlaceholderText(tr("No implementation details available."));
    stepsText_->setOpenExternalLinks(true);

    splitter->addWidget(methodologyTree_);
    splitter->addWidget(stepsText_);
    splitter->setStretchFactor(0, 2);
    splitter->setStretchFactor(1, 1);

    layout->addWidget(splitter);
    return widget;
}

QWidget* DatasetViewDialog::createLineageTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(8, 8, 8, 8);

    lineageView_ = new QGraphicsView();
    lineageView_->setScene(new QGraphicsScene(lineageView_));
    lineageView_->setRenderHint(QPainter::Antialiasing);
    lineageView_->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    lineageView_->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

    layout->addWidget(lineageView_);
    return widget;
}

void DatasetViewDialog::addProperty(QTreeWidget* tree, const QString& name,
    const QString& value, const QString& tooltip) {

    auto* item = new QTreeWidgetItem(tree);
    item->setText(0, name);
    item->setText(1, value);
    item->setFlags(item->flags() | Qt::ItemNeverHasChildren);

    // Make property name bold for better readability
    auto nameFont = item->font(0);
    nameFont.setBold(true);
    item->setFont(0, nameFont);

    if (!tooltip.isEmpty()) {
        item->setToolTip(0, tooltip);
        item->setToolTip(1, tooltip);
    }
}

void DatasetViewDialog::addSectionHeader(QTreeWidget* tree, const QString& title) {
    auto* item = new QTreeWidgetItem(tree);
    item->setFlags(item->flags() | Qt::ItemNeverHasChildren);
    item->setFirstColumnSpanned(true);

    // Style section header: centered, bold, larger, distinct gray background
    auto font = item->font(0);
    font.setBold(true);
    font.setPointSize(font.pointSize() + 1);
    item->setFont(0, font);
    item->setText(0, title);
    item->setTextAlignment(0, Qt::AlignCenter);
    item->setBackground(0, tree->palette().color(QPalette::AlternateBase));
    item->setForeground(0, tree->palette().color(QPalette::Text));
}

QString DatasetViewDialog::findMethodologyName(
    const std::optional<boost::uuids::uuid>& methodologyId) const {
    if (!methodologyId) {
        return tr("None");
    }
    for (const auto& m : methodologies_) {
        if (m.id == *methodologyId) {
            return QString::fromStdString(m.name);
        }
    }
    return tr("Unknown");
}

const dq::domain::methodology* DatasetViewDialog::findMethodology(
    const std::optional<boost::uuids::uuid>& methodologyId) const {
    if (!methodologyId) {
        return nullptr;
    }
    for (const auto& m : methodologies_) {
        if (m.id == *methodologyId) {
            return &m;
        }
    }
    return nullptr;
}

void DatasetViewDialog::setDataset(const dq::domain::dataset& dataset) {
    dataset_ = dataset;
    setWindowTitle(tr("Dataset: %1").arg(QString::fromStdString(dataset_.name)));

    updateOverviewTab();
    updateProvenanceTab();
    updateMethodologyTab();
    updateLineageView();
}

void DatasetViewDialog::setMethodologies(const std::vector<dq::domain::methodology>& methodologies) {
    methodologies_ = methodologies;
}

void DatasetViewDialog::setCatalogDependencies(
    const std::vector<dq::domain::catalog_dependency>& dependencies) {
    catalogDependencies_ = dependencies;
}

void DatasetViewDialog::updateOverviewTab() {
    overviewTree_->clear();

    // General section
    addSectionHeader(overviewTree_, tr("General"));
    addProperty(overviewTree_, tr("Name"), QString::fromStdString(dataset_.name));
    addProperty(overviewTree_, tr("Version"), QString::number(dataset_.version));
    addProperty(overviewTree_, tr("ID"),
        QString::fromStdString(boost::uuids::to_string(dataset_.id)));
    addProperty(overviewTree_, tr("Description"),
        dataset_.description.empty() ? tr("-") : QString::fromStdString(dataset_.description));

    // Classification section
    addSectionHeader(overviewTree_, tr("Classification"));
    addProperty(overviewTree_, tr("Domain"), QString::fromStdString(dataset_.domain_name));
    addProperty(overviewTree_, tr("Subject Area"),
        QString::fromStdString(dataset_.subject_area_name));
    addProperty(overviewTree_, tr("Catalog"),
        dataset_.catalog_name ? QString::fromStdString(*dataset_.catalog_name) : tr("-"));

    // Data Quality section
    addSectionHeader(overviewTree_, tr("Data Quality"));
    addProperty(overviewTree_, tr("Origin"), QString::fromStdString(dataset_.origin_code));
    addProperty(overviewTree_, tr("Nature"), QString::fromStdString(dataset_.nature_code));
    addProperty(overviewTree_, tr("Treatment"), QString::fromStdString(dataset_.treatment_code));

    // Audit section
    addSectionHeader(overviewTree_, tr("Audit"));
    addProperty(overviewTree_, tr("Recorded By"), QString::fromStdString(dataset_.recorded_by));
    addProperty(overviewTree_, tr("Recorded At"),
        relative_time_helper::format(dataset_.recorded_at));
    addProperty(overviewTree_, tr("Commentary"),
        dataset_.change_commentary.empty()
            ? tr("-") : QString::fromStdString(dataset_.change_commentary));
}

void DatasetViewDialog::updateProvenanceTab() {
    provenanceTree_->clear();

    // Source section
    addSectionHeader(provenanceTree_, tr("Source"));
    addProperty(provenanceTree_, tr("Source System"),
        QString::fromStdString(dataset_.source_system_id));
    addProperty(provenanceTree_, tr("Business Context"),
        dataset_.business_context.empty()
            ? tr("-") : QString::fromStdString(dataset_.business_context));
    addProperty(provenanceTree_, tr("License"),
        dataset_.license_info ? QString::fromStdString(*dataset_.license_info) : tr("-"));
    addProperty(provenanceTree_, tr("Coding Scheme"),
        dataset_.coding_scheme_code
            ? QString::fromStdString(*dataset_.coding_scheme_code) : tr("-"));

    // Dates section
    addSectionHeader(provenanceTree_, tr("Dates"));
    addProperty(provenanceTree_, tr("As Of Date"),
        relative_time_helper::format(dataset_.as_of_date));
    addProperty(provenanceTree_, tr("Ingestion"),
        relative_time_helper::format(dataset_.ingestion_timestamp));

    // Lineage section
    addSectionHeader(provenanceTree_, tr("Lineage"));
    addProperty(provenanceTree_, tr("Upstream Derivation"),
        dataset_.upstream_derivation_id
            ? QString::fromStdString(boost::uuids::to_string(*dataset_.upstream_derivation_id))
            : tr("-"));
    addProperty(provenanceTree_, tr("Lineage Depth"), QString::number(dataset_.lineage_depth));
}

void DatasetViewDialog::updateMethodologyTab() {
    methodologyTree_->clear();
    stepsText_->clear();

    const auto* methodology = findMethodology(dataset_.methodology_id);
    if (!methodology) {
        addProperty(methodologyTree_, tr("Status"), tr("No methodology assigned"));
        stepsText_->setPlainText(tr("No methodology is assigned to this dataset."));
        return;
    }

    // Basic info
    addSectionHeader(methodologyTree_, tr("General"));
    addProperty(methodologyTree_, tr("Name"), QString::fromStdString(methodology->name));
    addProperty(methodologyTree_, tr("Version"), QString::number(methodology->version));
    addProperty(methodologyTree_, tr("ID"),
        QString::fromStdString(boost::uuids::to_string(methodology->id)));
    addProperty(methodologyTree_, tr("Description"),
        methodology->description.empty()
            ? tr("-") : QString::fromStdString(methodology->description));

    // References
    addSectionHeader(methodologyTree_, tr("References"));
    addProperty(methodologyTree_, tr("Logic Reference"),
        methodology->logic_reference
            ? QString::fromStdString(*methodology->logic_reference) : tr("-"));

    // Audit info
    addSectionHeader(methodologyTree_, tr("Audit"));
    addProperty(methodologyTree_, tr("Recorded By"),
        QString::fromStdString(methodology->recorded_by));
    addProperty(methodologyTree_, tr("Recorded At"),
        relative_time_helper::format(methodology->recorded_at));
    addProperty(methodologyTree_, tr("Commentary"),
        methodology->change_commentary.empty()
            ? tr("-") : QString::fromStdString(methodology->change_commentary));

    // Implementation details in the text browser
    if (methodology->implementation_details) {
        stepsText_->setPlainText(QString::fromStdString(*methodology->implementation_details));
    } else {
        stepsText_->setPlainText(tr("No implementation details available."));
    }
}

void DatasetViewDialog::updateLineageView() {
    auto* scene = lineageView_->scene();
    scene->clear();

    // Layout parameters
    const qreal rowSpacing = 50;  // Vertical spacing between rows
    qreal currentX = 0;
    qreal catalogRow = 0;
    qreal datasetRow = rowSpacing + lineageHeaderHeight_ + lineageRowHeight_ * 3;

    std::vector<std::pair<qreal, qreal>> catalogNodeCenters;  // X, Y for connections

    // Row 1: Catalog dependencies (if dataset has a catalog)
    if (dataset_.catalog_name.has_value() && !catalogDependencies_.empty()) {
        const std::string& datasetCatalog = *dataset_.catalog_name;

        // Find dependencies where the dataset's catalog depends on others
        std::vector<std::string> dependencyNames;
        for (const auto& dep : catalogDependencies_) {
            if (dep.catalog_name == datasetCatalog) {
                dependencyNames.push_back(dep.dependency_name);
            }
        }

        // Create nodes for dependency catalogs
        for (const auto& depName : dependencyNames) {
            qreal height = createLineageNode(scene, currentX, catalogRow,
                tr("Catalog"),
                {tr("Name")},
                {QString::fromStdString(depName)},
                lineageHeaderCatalog_, false, true);

            catalogNodeCenters.emplace_back(
                currentX + lineageNodeWidth_ + lineageSocketRadius_,
                catalogRow + height / 2);

            currentX += lineageNodeWidth_ + lineageNodeSpacing_;
        }

        // Create node for dataset's catalog
        if (!dependencyNames.empty()) {
            qreal catalogHeight = createLineageNode(scene, currentX, catalogRow,
                tr("Catalog"),
                {tr("Name")},
                {QString::fromStdString(datasetCatalog)},
                lineageHeaderCatalog_, true, true);

            qreal catalogCenterY = catalogRow + catalogHeight / 2;

            // Draw connections from dependency catalogs to dataset's catalog
            for (const auto& [depX, depY] : catalogNodeCenters) {
                drawLineageConnection(scene, depX, depY,
                    currentX - lineageSocketRadius_, catalogCenterY);
            }

            // Store for connection to dataset
            catalogNodeCenters.clear();
            catalogNodeCenters.emplace_back(
                currentX + lineageNodeWidth_ / 2,
                catalogRow + catalogHeight);

            currentX += lineageNodeWidth_ + lineageNodeSpacing_;
        }
    } else if (dataset_.catalog_name.has_value()) {
        // Dataset has a catalog but no dependencies - show just the catalog
        qreal height = createLineageNode(scene, currentX, catalogRow,
            tr("Catalog"),
            {tr("Name")},
            {QString::fromStdString(*dataset_.catalog_name)},
            lineageHeaderCatalog_, false, true);

        catalogNodeCenters.emplace_back(
            currentX + lineageNodeWidth_ / 2,
            catalogRow + height);

        currentX += lineageNodeWidth_ + lineageNodeSpacing_;
    }

    // Row 2: Dataset node
    qreal datasetX = 0;
    if (!catalogNodeCenters.empty()) {
        // Center dataset under the last catalog node
        datasetX = catalogNodeCenters.back().first - lineageNodeWidth_ / 2;
    }

    qreal datasetHeight = createLineageNode(scene, datasetX, datasetRow,
        tr("Dataset"),
        {tr("Name"), tr("Origin"), tr("Nature"), tr("Treatment")},
        {QString::fromStdString(dataset_.name),
         QString::fromStdString(dataset_.origin_code),
         QString::fromStdString(dataset_.nature_code),
         QString::fromStdString(dataset_.treatment_code)},
        lineageHeaderDataset_, !catalogNodeCenters.empty(), false);

    // Draw connection from catalog to dataset
    if (!catalogNodeCenters.empty()) {
        qreal datasetCenterX = datasetX + lineageNodeWidth_ / 2;
        drawLineageConnection(scene,
            catalogNodeCenters.back().first,
            catalogNodeCenters.back().second,
            datasetCenterX,
            datasetRow - lineageSocketRadius_);
    }

    // Set scene background
    scene->setBackgroundBrush(QBrush(lineageBackground_));

    // Fit view with some margin
    QRectF sceneRect = scene->itemsBoundingRect();
    sceneRect.adjust(-16, -16, 16, 16);
    lineageView_->fitInView(sceneRect, Qt::KeepAspectRatio);
}

qreal DatasetViewDialog::createLineageNode(QGraphicsScene* scene, qreal x, qreal y,
    const QString& headerText, const QStringList& labels, const QStringList& values,
    const QColor& headerColor, bool hasInputSocket, bool hasOutputSocket) const {

    const int numRows = std::min(labels.size(), values.size());
    const qreal bodyHeight = numRows * lineageRowHeight_ + lineagePadding_ * 2;
    const qreal nodeHeight = lineageHeaderHeight_ + bodyHeight;

    // Build tooltip with all properties
    QString tooltip = QString("<b>%1</b>").arg(headerText);
    for (int i = 0; i < numRows; ++i) {
        tooltip += QString("<br>%1: %2").arg(labels[i], values[i]);
    }

    createLineageNodeBody(scene, x, y, nodeHeight, tooltip);
    createLineageNodeHeader(scene, x, y, headerText, headerColor, tooltip);
    createLineageNodeProperties(scene, x, y + lineageHeaderHeight_ + lineagePadding_,
        labels, values);
    createLineageNodeSockets(scene, x, y, nodeHeight, hasInputSocket, hasOutputSocket);

    return nodeHeight;
}

void DatasetViewDialog::createLineageNodeBody(QGraphicsScene* scene, qreal x, qreal y,
    qreal nodeHeight, const QString& tooltip) const {

    QPainterPath bodyPath;
    bodyPath.addRoundedRect(x, y, lineageNodeWidth_, nodeHeight,
        lineageCornerRadius_, lineageCornerRadius_);
    auto* bodyItem = scene->addPath(bodyPath,
        QPen(lineageNodeBorder_), QBrush(lineageNodeBody_));
    bodyItem->setToolTip(tooltip);
}

void DatasetViewDialog::createLineageNodeHeader(QGraphicsScene* scene, qreal x, qreal y,
    const QString& headerText, const QColor& headerColor,
    const QString& tooltip) const {

    // Header background (top rounded, bottom square)
    QPainterPath headerPath;
    headerPath.moveTo(x + lineageCornerRadius_, y);
    headerPath.arcTo(x, y, lineageCornerRadius_ * 2, lineageCornerRadius_ * 2, 90, 90);
    headerPath.lineTo(x, y + lineageHeaderHeight_);
    headerPath.lineTo(x + lineageNodeWidth_, y + lineageHeaderHeight_);
    headerPath.lineTo(x + lineageNodeWidth_, y + lineageCornerRadius_);
    headerPath.arcTo(x + lineageNodeWidth_ - lineageCornerRadius_ * 2, y,
        lineageCornerRadius_ * 2, lineageCornerRadius_ * 2, 0, 90);
    headerPath.closeSubpath();
    auto* headerBg = scene->addPath(headerPath, QPen(Qt::NoPen), QBrush(headerColor));
    headerBg->setToolTip(tooltip);

    // Header text (centered)
    QFont headerFont;
    headerFont.setPointSize(6);
    headerFont.setBold(true);
    QFontMetrics headerFm(headerFont);

    QString elidedHeader = headerFm.elidedText(headerText, Qt::ElideRight,
        static_cast<int>(lineageNodeWidth_ - 2 * lineagePadding_));
    auto* headerItem = scene->addText(elidedHeader);
    headerItem->setFont(headerFont);
    headerItem->setDefaultTextColor(lineageText_);
    headerItem->setToolTip(tooltip);
    qreal headerTextWidth = headerFm.horizontalAdvance(elidedHeader);
    headerItem->setPos(x + (lineageNodeWidth_ - headerTextWidth) / 2,
        y + (lineageHeaderHeight_ - headerFm.height()) / 2 - 1);
}

void DatasetViewDialog::createLineageNodeProperties(QGraphicsScene* scene, qreal x, qreal y,
    const QStringList& labels, const QStringList& values) const {

    QFont labelFont;
    labelFont.setPointSize(5);
    QFont valueFont;
    valueFont.setPointSize(5);
    valueFont.setBold(true);
    QFontMetrics labelFm(labelFont);
    QFontMetrics valueFm(valueFont);

    int maxLabelWidth = static_cast<int>(lineageNodeWidth_ * 0.35);
    int maxValueWidth = static_cast<int>(lineageNodeWidth_ - maxLabelWidth - lineagePadding_ * 3);
    int numRows = std::min(labels.size(), values.size());

    qreal rowY = y;
    for (int i = 0; i < numRows; ++i) {
        // Label (left aligned)
        QString elidedLabel = labelFm.elidedText(labels[i] + ":", Qt::ElideRight, maxLabelWidth);
        auto* labelItem = scene->addText(elidedLabel);
        labelItem->setFont(labelFont);
        labelItem->setDefaultTextColor(lineageLabel_);
        labelItem->setPos(x + lineagePadding_, rowY + (lineageRowHeight_ - labelFm.height()) / 2);
        labelItem->setToolTip(QString("%1: %2").arg(labels[i], values[i]));

        // Value (right of label)
        QString elidedValue = valueFm.elidedText(values[i], Qt::ElideRight, maxValueWidth);
        auto* valueItem = scene->addText(elidedValue);
        valueItem->setFont(valueFont);
        valueItem->setDefaultTextColor(lineageValue_);
        valueItem->setPos(x + lineagePadding_ + maxLabelWidth + 2,
            rowY + (lineageRowHeight_ - valueFm.height()) / 2);
        valueItem->setToolTip(QString("%1: %2").arg(labels[i], values[i]));

        rowY += lineageRowHeight_;
    }
}

void DatasetViewDialog::createLineageNodeSockets(QGraphicsScene* scene, qreal x, qreal y,
    qreal nodeHeight, bool hasInput, bool hasOutput) const {

    qreal socketY = y + nodeHeight / 2;

    if (hasInput) {
        scene->addEllipse(x - lineageSocketRadius_, socketY - lineageSocketRadius_,
            lineageSocketRadius_ * 2, lineageSocketRadius_ * 2,
            QPen(lineageNodeBorder_), QBrush(lineageSocket_));
    }

    if (hasOutput) {
        scene->addEllipse(x + lineageNodeWidth_ - lineageSocketRadius_, socketY - lineageSocketRadius_,
            lineageSocketRadius_ * 2, lineageSocketRadius_ * 2,
            QPen(lineageNodeBorder_), QBrush(lineageSocket_));
    }
}

void DatasetViewDialog::drawLineageConnection(QGraphicsScene* scene,
    qreal x1, qreal y1, qreal x2, qreal y2) const {

    QPainterPath path;
    path.moveTo(x1, y1);
    qreal ctrlOffset = (x2 - x1) * 0.4;
    path.cubicTo(x1 + ctrlOffset, y1, x2 - ctrlOffset, y2, x2, y2);
    scene->addPath(path, QPen(lineageConnection_, 1.2));
}

}
