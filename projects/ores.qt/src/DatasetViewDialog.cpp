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
#include "ores.qt/RelativeTimeHelper.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QScrollArea>
#include <QPainterPath>
#include <QGraphicsTextItem>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

DatasetViewDialog::DatasetViewDialog(QWidget* parent)
    : QDialog(parent) {
    setupUi();
}

DatasetViewDialog::~DatasetViewDialog() = default;

void DatasetViewDialog::setupUi() {
    setWindowTitle(tr("Dataset Details"));
    setMinimumSize(600, 500);
    resize(700, 550);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    // Create tab widget with tabs on the left (West)
    tabWidget_ = new QTabWidget(this);
    tabWidget_->setTabPosition(QTabWidget::West);

    // Add tabs
    tabWidget_->addTab(createGeneralTab(), tr("General"));
    tabWidget_->addTab(createClassificationTab(), tr("Classification"));
    tabWidget_->addTab(createDataQualityTab(), tr("Data Quality"));
    tabWidget_->addTab(createProvenanceTab(), tr("Provenance"));
    tabWidget_->addTab(createLineageTab(), tr("Lineage"));
    tabWidget_->addTab(createAuditTab(), tr("Audit"));

    mainLayout->addWidget(tabWidget_);
}

QWidget* DatasetViewDialog::createGeneralTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(16, 16, 16, 16);

    auto* form = new QFormLayout();
    form->setLabelAlignment(Qt::AlignRight);
    form->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    nameLabel_ = new QLabel();
    nameLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Name:"), nameLabel_);

    versionLabel_ = new QLabel();
    versionLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Version:"), versionLabel_);

    idLabel_ = new QLabel();
    idLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    idLabel_->setWordWrap(true);
    form->addRow(tr("ID:"), idLabel_);

    catalogLabel_ = new QLabel();
    catalogLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Catalog:"), catalogLabel_);

    descriptionLabel_ = new QLabel();
    descriptionLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    descriptionLabel_->setWordWrap(true);
    form->addRow(tr("Description:"), descriptionLabel_);

    layout->addLayout(form);
    layout->addStretch();
    return widget;
}

QWidget* DatasetViewDialog::createClassificationTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(16, 16, 16, 16);

    auto* form = new QFormLayout();
    form->setLabelAlignment(Qt::AlignRight);
    form->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    domainLabel_ = new QLabel();
    domainLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Domain:"), domainLabel_);

    subjectAreaLabel_ = new QLabel();
    subjectAreaLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Subject Area:"), subjectAreaLabel_);

    layout->addLayout(form);
    layout->addStretch();
    return widget;
}

QWidget* DatasetViewDialog::createDataQualityTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(16, 16, 16, 16);

    auto* form = new QFormLayout();
    form->setLabelAlignment(Qt::AlignRight);
    form->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    originLabel_ = new QLabel();
    originLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Origin:"), originLabel_);

    natureLabel_ = new QLabel();
    natureLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Nature:"), natureLabel_);

    treatmentLabel_ = new QLabel();
    treatmentLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Treatment:"), treatmentLabel_);

    layout->addLayout(form);
    layout->addStretch();
    return widget;
}

QWidget* DatasetViewDialog::createProvenanceTab() {
    auto* scrollArea = new QScrollArea();
    scrollArea->setWidgetResizable(true);
    scrollArea->setFrameShape(QFrame::NoFrame);

    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(16, 16, 16, 16);

    auto* form = new QFormLayout();
    form->setLabelAlignment(Qt::AlignRight);
    form->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    methodologyLabel_ = new QLabel();
    methodologyLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Methodology:"), methodologyLabel_);

    sourceSystemLabel_ = new QLabel();
    sourceSystemLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Source System:"), sourceSystemLabel_);

    asOfDateLabel_ = new QLabel();
    asOfDateLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("As Of Date:"), asOfDateLabel_);

    ingestionLabel_ = new QLabel();
    ingestionLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Ingestion:"), ingestionLabel_);

    licenseLabel_ = new QLabel();
    licenseLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    licenseLabel_->setWordWrap(true);
    form->addRow(tr("License:"), licenseLabel_);

    codingSchemeLabel_ = new QLabel();
    codingSchemeLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Coding Scheme:"), codingSchemeLabel_);

    businessContextLabel_ = new QLabel();
    businessContextLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    businessContextLabel_->setWordWrap(true);
    form->addRow(tr("Business Context:"), businessContextLabel_);

    upstreamLabel_ = new QLabel();
    upstreamLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    upstreamLabel_->setWordWrap(true);
    form->addRow(tr("Upstream:"), upstreamLabel_);

    lineageDepthLabel_ = new QLabel();
    lineageDepthLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Lineage Depth:"), lineageDepthLabel_);

    layout->addLayout(form);
    layout->addStretch();

    scrollArea->setWidget(widget);
    return scrollArea;
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

QWidget* DatasetViewDialog::createAuditTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(16, 16, 16, 16);

    auto* form = new QFormLayout();
    form->setLabelAlignment(Qt::AlignRight);
    form->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    recordedByLabel_ = new QLabel();
    recordedByLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Recorded By:"), recordedByLabel_);

    recordedAtLabel_ = new QLabel();
    recordedAtLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(tr("Recorded At:"), recordedAtLabel_);

    commentaryLabel_ = new QLabel();
    commentaryLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    commentaryLabel_->setWordWrap(true);
    form->addRow(tr("Commentary:"), commentaryLabel_);

    layout->addLayout(form);
    layout->addStretch();
    return widget;
}

void DatasetViewDialog::setDataset(const dq::domain::dataset& dataset) {
    dataset_ = dataset;
    setWindowTitle(tr("Dataset: %1").arg(QString::fromStdString(dataset_.name)));

    updateGeneralTab();
    updateClassificationTab();
    updateDataQualityTab();
    updateProvenanceTab();
    updateLineageView();
    updateAuditTab();
}

void DatasetViewDialog::setMethodologies(const std::vector<dq::domain::methodology>& methodologies) {
    methodologies_ = methodologies;
}

void DatasetViewDialog::updateGeneralTab() {
    nameLabel_->setText(QString::fromStdString(dataset_.name));
    versionLabel_->setText(QString::number(dataset_.version));
    idLabel_->setText(QString::fromStdString(boost::uuids::to_string(dataset_.id)));
    catalogLabel_->setText(dataset_.catalog_name
        ? QString::fromStdString(*dataset_.catalog_name)
        : tr("-"));
    descriptionLabel_->setText(dataset_.description.empty()
        ? tr("-")
        : QString::fromStdString(dataset_.description));
}

void DatasetViewDialog::updateClassificationTab() {
    domainLabel_->setText(QString::fromStdString(dataset_.domain_name));
    subjectAreaLabel_->setText(QString::fromStdString(dataset_.subject_area_name));
}

void DatasetViewDialog::updateDataQualityTab() {
    originLabel_->setText(QString::fromStdString(dataset_.origin_code));
    natureLabel_->setText(QString::fromStdString(dataset_.nature_code));
    treatmentLabel_->setText(QString::fromStdString(dataset_.treatment_code));
}

void DatasetViewDialog::updateProvenanceTab() {
    methodologyLabel_->setText(findMethodologyName(dataset_.methodology_id));
    sourceSystemLabel_->setText(QString::fromStdString(dataset_.source_system_id));
    asOfDateLabel_->setText(relative_time_helper::format(dataset_.as_of_date));
    ingestionLabel_->setText(relative_time_helper::format(dataset_.ingestion_timestamp));
    licenseLabel_->setText(dataset_.license_info
        ? QString::fromStdString(*dataset_.license_info)
        : tr("-"));
    codingSchemeLabel_->setText(dataset_.coding_scheme_code
        ? QString::fromStdString(*dataset_.coding_scheme_code)
        : tr("-"));
    businessContextLabel_->setText(dataset_.business_context.empty()
        ? tr("-")
        : QString::fromStdString(dataset_.business_context));
    upstreamLabel_->setText(dataset_.upstream_derivation_id
        ? QString::fromStdString(boost::uuids::to_string(*dataset_.upstream_derivation_id))
        : tr("-"));
    lineageDepthLabel_->setText(QString::number(dataset_.lineage_depth));
}

void DatasetViewDialog::updateAuditTab() {
    recordedByLabel_->setText(QString::fromStdString(dataset_.recorded_by));
    recordedAtLabel_->setText(relative_time_helper::format(dataset_.recorded_at));
    commentaryLabel_->setText(dataset_.change_commentary.empty()
        ? tr("-")
        : QString::fromStdString(dataset_.change_commentary));
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

void DatasetViewDialog::updateLineageView() {
    auto* scene = lineageView_->scene();
    scene->clear();

    // Calculate node positions using member dimension properties
    const qreal node1X = 0;
    const qreal node2X = lineageNodeWidth_ + lineageNodeSpacing_;
    const qreal node3X = 2 * (lineageNodeWidth_ + lineageNodeSpacing_);
    const qreal nodeY = 0;

    // Create nodes using helper methods
    qreal node1Height = createLineageNode(scene, node1X, nodeY, tr("Primary"),
        {tr("Origin"), tr("Nature"), tr("Treatment")},
        {QString::fromStdString(dataset_.origin_code),
         QString::fromStdString(dataset_.nature_code),
         QString::fromStdString(dataset_.treatment_code)},
        lineageHeaderOrigin_, false, true);

    qreal node2Height = createLineageNode(scene, node2X, nodeY, tr("Process"),
        {tr("Method"), tr("System")},
        {findMethodologyName(dataset_.methodology_id),
         QString::fromStdString(dataset_.source_system_id)},
        lineageHeaderMethod_, true, true);

    qreal node3Height = createLineageNode(scene, node3X, nodeY, tr("Output"),
        {tr("Name"), tr("Version"), tr("As Of")},
        {QString::fromStdString(dataset_.name),
         QString::number(dataset_.version),
         relative_time_helper::format(dataset_.as_of_date)},
        lineageHeaderDataset_, true, false);

    // Draw connections between nodes
    qreal connY1 = nodeY + node1Height / 2;
    qreal connY2 = nodeY + node2Height / 2;
    qreal connY3 = nodeY + node3Height / 2;

    drawLineageConnection(scene,
        node1X + lineageNodeWidth_ + lineageSocketRadius_, connY1,
        node2X - lineageSocketRadius_, connY2);
    drawLineageConnection(scene,
        node2X + lineageNodeWidth_ + lineageSocketRadius_, connY2,
        node3X - lineageSocketRadius_, connY3);

    // Set scene background
    scene->setBackgroundBrush(QBrush(lineageBackground_));

    // Fit view with some margin
    QRectF sceneRect = scene->itemsBoundingRect();
    sceneRect.adjust(-8, -8, 8, 8);
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
