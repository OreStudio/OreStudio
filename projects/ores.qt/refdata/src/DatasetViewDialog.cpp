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
#include "ores.qt/BadgeLabelUtils.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include <QApplication>
#include <QClipboard>
#include <QFrame>
#include <QGraphicsTextItem>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QPainterPath>
#include <QPushButton>
#include <QSizePolicy>
#include <QSplitter>
#include <QVBoxLayout>
#include <QWheelEvent>
#include <boost/uuid/uuid_io.hpp>
#include <cmath>

namespace ores::qt {

/**
 * @brief QGraphicsView with mouse-wheel zoom and click-and-drag pan, plus a
 * way back to the original fitInView framing.
 */
class LineageGraphView : public QGraphicsView {
public:
    using QGraphicsView::QGraphicsView;

    void setFitRect(const QRectF& rect) {
        fitRect_ = rect;
    }

    void resetToFit() {
        if (!fitRect_.isNull()) {
            resetTransform();
            fitInView(fitRect_, Qt::KeepAspectRatio);
        }
    }

protected:
    void wheelEvent(QWheelEvent* event) override {
        constexpr double zoomInFactor = 1.15;
        const double factor = event->angleDelta().y() > 0 ? zoomInFactor : 1.0 / zoomInFactor;
        scale(factor, factor);
    }

private:
    QRectF fitRect_;
};

DatasetViewDialog::DatasetViewDialog(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager) {
    setupUi();
}

DatasetViewDialog::~DatasetViewDialog() = default;

void DatasetViewDialog::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    setWindowTitle(tr("Dataset Details"));
    setMinimumSize(800, 600);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    mainLayout->addWidget(createHeaderBanner());

    // Create tab widget with tabs at the top
    tabWidget_ = new QTabWidget(this);

    // Add tabs
    tabWidget_->addTab(createOverviewTab(), tr("Overview"));
    tabWidget_->addTab(createProvenanceAndMethodologyTab(), tr("Provenance && Methodology"));
    tabWidget_->addTab(createLineageTab(), tr("Dependencies"));

    mainLayout->addWidget(tabWidget_);
}

QWidget* DatasetViewDialog::createHeaderBanner() {
    auto* frame = new QFrame();
    frame->setFrameShape(QFrame::StyledPanel);
    frame->setObjectName("datasetHeaderBanner");
    // No background fill: palette(alternate-base) is meant for tight
    // alternating list rows and renders near-black in this dark theme —
    // a subtle separator border reads as "persistent header" without
    // fighting the app's own palette.
    frame->setStyleSheet("#datasetHeaderBanner { border-bottom: 1px solid palette(mid); }");

    auto* layout = new QHBoxLayout(frame);
    layout->setContentsMargins(12, 8, 12, 8);

    auto* textLayout = new QVBoxLayout();
    headerNameLabel_ = new QLabel();
    auto nameFont = headerNameLabel_->font();
    nameFont.setPointSize(nameFont.pointSize() + 3);
    nameFont.setBold(true);
    headerNameLabel_->setFont(nameFont);

    headerCodeVersionLabel_ = new QLabel();

    headerIdLabel_ = new QLabel();
    headerIdLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);

    textLayout->addWidget(headerNameLabel_);
    textLayout->addWidget(headerCodeVersionLabel_);
    textLayout->addWidget(headerIdLabel_);
    layout->addLayout(textLayout);
    layout->addStretch();

    auto* copyButton = new QPushButton(tr("Copy ID"));
    connect(copyButton, &QPushButton::clicked, this, &DatasetViewDialog::onCopyIdClicked);
    layout->addWidget(copyButton, 0, Qt::AlignTop);

    return frame;
}

void DatasetViewDialog::onCopyIdClicked() {
    QApplication::clipboard()->setText(
        QString::fromStdString(boost::uuids::to_string(dataset_.id)));
}

void DatasetViewDialog::updateHeaderBanner() {
    headerNameLabel_->setText(QString::fromStdString(dataset_.name));
    headerCodeVersionLabel_->setText(
        tr("%1  |  v%2")
            .arg(QString::fromStdString(dataset_.code))
            .arg(dataset_.version));
    const auto idString = boost::uuids::to_string(dataset_.id);
    headerIdLabel_->setText(tr("ID: %1").arg(QString::fromStdString(idString)));
}

QFormLayout* DatasetViewDialog::addPropertyCard(QBoxLayout* parentLayout, const QString& title) {
    auto* box = new QGroupBox(title);
    auto* form = new QFormLayout(box);
    parentLayout->addWidget(box);
    return form;
}

QLabel* DatasetViewDialog::addProseCard(QBoxLayout* parentLayout, const QString& title) {
    auto* box = new QGroupBox(title);
    auto* layout = new QVBoxLayout(box);
    auto* label = new QLabel();
    label->setWordWrap(true);
    label->setTextInteractionFlags(Qt::TextSelectableByMouse);
    layout->addWidget(label);
    parentLayout->addWidget(box);
    return label;
}

void DatasetViewDialog::addFormRow(QFormLayout* form, const QString& name, const QString& value) {
    auto* valueLabel = new QLabel(value);
    valueLabel->setWordWrap(true);
    valueLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    form->addRow(name, valueLabel);
}

QLabel* DatasetViewDialog::addBadgePlaceholder(QFormLayout* form, const QString& name) {
    // QFormLayout stretches its field column to fill the card's width, so a
    // bare badge QLabel paints its background across the whole row instead
    // of hugging its text. Wrap it in a container that left-aligns the
    // badge and absorbs the rest of the width via a trailing stretch —
    // same fix as the QTreeWidget badge cells used before this rework.
    auto* container = new QWidget();
    auto* containerLayout = new QHBoxLayout(container);
    containerLayout->setContentsMargins(0, 0, 0, 0);
    auto* badge = new QLabel(container);
    badge->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    containerLayout->addWidget(badge);
    containerLayout->addStretch();
    form->addRow(name, container);
    return badge;
}

QWidget* DatasetViewDialog::createOverviewTab() {
    auto* splitter = new QSplitter(Qt::Horizontal);

    // Primary stage (~65%): prose.
    auto* primary = new QWidget();
    auto* primaryLayout = new QVBoxLayout(primary);
    overviewDescriptionLabel_ = addProseCard(primaryLayout, tr("Description"));
    overviewCommentaryLabel_ = addProseCard(primaryLayout, tr("Commentary"));
    primaryLayout->addStretch();

    // Sidebar (~35%): metadata cards.
    auto* sidebar = new QWidget();
    auto* sidebarLayout = new QVBoxLayout(sidebar);

    auto* classificationForm = addPropertyCard(sidebarLayout, tr("Classification"));
    overviewDomainLabel_ = new QLabel();
    classificationForm->addRow(tr("Domain"), overviewDomainLabel_);
    overviewSubjectAreaLabel_ = new QLabel();
    classificationForm->addRow(tr("Subject Area"), overviewSubjectAreaLabel_);
    overviewCatalogLabel_ = new QLabel();
    classificationForm->addRow(tr("Catalog"), overviewCatalogLabel_);

    auto* governanceForm = addPropertyCard(sidebarLayout, tr("Data Governance"));
    overviewOriginBadge_ = addBadgePlaceholder(governanceForm, tr("Origin"));
    overviewNatureBadge_ = addBadgePlaceholder(governanceForm, tr("Nature"));
    overviewTreatmentBadge_ = addBadgePlaceholder(governanceForm, tr("Treatment"));

    auto* auditForm = addPropertyCard(sidebarLayout, tr("Audit"));
    overviewModifiedByLabel_ = new QLabel();
    auditForm->addRow(tr("Modified By"), overviewModifiedByLabel_);
    overviewRecordedAtLabel_ = new QLabel();
    auditForm->addRow(tr("Recorded At"), overviewRecordedAtLabel_);

    sidebarLayout->addStretch();

    splitter->addWidget(primary);
    splitter->addWidget(sidebar);
    splitter->setStretchFactor(0, 65);
    splitter->setStretchFactor(1, 35);
    // setStretchFactor only governs how *extra* space is redistributed on
    // resize; the initial split comes from sizeHint()/equal-split unless
    // pinned explicitly (see DataLibrarianWindow.cpp, MarketSimulatorWindow.cpp).
    splitter->setSizes({650, 350});
    return splitter;
}

QWidget* DatasetViewDialog::createProvenanceAndMethodologyTab() {
    auto* splitter = new QSplitter(Qt::Horizontal);

    // Primary stage (~65%): methodology/business-context prose, lifecycle
    // timeline, and implementation details.
    auto* primary = new QWidget();
    auto* primaryLayout = new QVBoxLayout(primary);
    methodologyDescriptionLabel_ = addProseCard(primaryLayout, tr("Methodology"));
    businessContextLabel_ = addProseCard(primaryLayout, tr("Business Context"));
    commentaryLabel_ = addProseCard(primaryLayout, tr("Commentary"));

    auto* timelineForm = addPropertyCard(primaryLayout, tr("Lifecycle Timeline"));
    asOfDateLabel_ = new QLabel();
    timelineForm->addRow(tr("As Of Date"), asOfDateLabel_);
    ingestionLabel_ = new QLabel();
    timelineForm->addRow(tr("Ingestion"), ingestionLabel_);

    auto* stepsBox = new QGroupBox(tr("Implementation Details"));
    auto* stepsBoxLayout = new QVBoxLayout(stepsBox);
    stepsText_ = new QTextBrowser();
    stepsText_->setPlaceholderText(tr("No implementation details available."));
    stepsText_->setOpenExternalLinks(true);
    stepsBoxLayout->addWidget(stepsText_);
    primaryLayout->addWidget(stepsBox);

    // Sidebar (~35%): source, lineage metrics, methodology identity, audit.
    auto* sidebar = new QWidget();
    auto* sidebarLayout = new QVBoxLayout(sidebar);

    auto* sourceForm = addPropertyCard(sidebarLayout, tr("Source"));
    sourceSystemLabel_ = new QLabel();
    sourceForm->addRow(tr("Source System"), sourceSystemLabel_);
    licenseLabel_ = new QLabel();
    sourceForm->addRow(tr("License"), licenseLabel_);
    codingSchemeLabel_ = new QLabel();
    sourceForm->addRow(tr("Coding Scheme"), codingSchemeLabel_);

    auto* lineageForm = addPropertyCard(sidebarLayout, tr("Lineage Metrics"));
    upstreamDerivationLabel_ = new QLabel();
    upstreamDerivationLabel_->setWordWrap(true);
    lineageForm->addRow(tr("Upstream Derivation"), upstreamDerivationLabel_);
    lineageDepthLabel_ = new QLabel();
    lineageForm->addRow(tr("Lineage Depth"), lineageDepthLabel_);

    auto* methodologyForm = addPropertyCard(sidebarLayout, tr("Methodology Info"));
    methodologyNameLabel_ = new QLabel();
    methodologyForm->addRow(tr("Name"), methodologyNameLabel_);
    methodologyVersionLabel_ = new QLabel();
    methodologyForm->addRow(tr("Version"), methodologyVersionLabel_);
    methodologyIdLabel_ = new QLabel();
    methodologyIdLabel_->setWordWrap(true);
    methodologyIdLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    methodologyForm->addRow(tr("ID"), methodologyIdLabel_);
    logicReferenceLabel_ = new QLabel();
    logicReferenceLabel_->setWordWrap(true);
    logicReferenceLabel_->setOpenExternalLinks(true);
    logicReferenceLabel_->setTextInteractionFlags(Qt::TextBrowserInteraction);
    methodologyForm->addRow(tr("Logic Reference"), logicReferenceLabel_);

    auto* auditForm = addPropertyCard(sidebarLayout, tr("Audit"));
    pmModifiedByLabel_ = new QLabel();
    auditForm->addRow(tr("Modified By"), pmModifiedByLabel_);
    pmRecordedAtLabel_ = new QLabel();
    auditForm->addRow(tr("Recorded At"), pmRecordedAtLabel_);

    sidebarLayout->addStretch();

    splitter->addWidget(primary);
    splitter->addWidget(sidebar);
    splitter->setStretchFactor(0, 65);
    splitter->setStretchFactor(1, 35);
    // setStretchFactor only governs how *extra* space is redistributed on
    // resize; the initial split comes from sizeHint()/equal-split unless
    // pinned explicitly (see DataLibrarianWindow.cpp, MarketSimulatorWindow.cpp).
    splitter->setSizes({650, 350});
    return splitter;
}

QWidget* DatasetViewDialog::createLineageTab() {
    auto* widget = new QWidget();
    auto* layout = new QVBoxLayout(widget);
    layout->setContentsMargins(8, 8, 8, 8);

    lineageView_ = new LineageGraphView();
    lineageView_->setScene(new QGraphicsScene(lineageView_));
    lineageView_->setRenderHint(QPainter::Antialiasing);
    lineageView_->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    lineageView_->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    lineageView_->setDragMode(QGraphicsView::ScrollHandDrag);
    lineageView_->setTransformationAnchor(QGraphicsView::AnchorUnderMouse);
    lineageView_->setResizeAnchor(QGraphicsView::AnchorUnderMouse);

    auto* toolbar = new QHBoxLayout();
    toolbar->addStretch();
    auto* resetButton = new QPushButton(tr("Reset View"));
    connect(resetButton, &QPushButton::clicked, lineageView_, &LineageGraphView::resetToFit);
    toolbar->addWidget(resetButton);

    layout->addLayout(toolbar);
    layout->addWidget(lineageView_);
    return widget;
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

const dq::domain::methodology*
DatasetViewDialog::findMethodology(const std::optional<boost::uuids::uuid>& methodologyId) const {
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

    updateHeaderBanner();
    updateOverviewTab();
    updateProvenanceAndMethodologyTab();
    updateLineageView();
}

void DatasetViewDialog::setMethodologies(
    const std::vector<dq::domain::methodology>& methodologies) {
    methodologies_ = methodologies;
}

void DatasetViewDialog::setDatasetDependencies(
    const std::vector<dq::domain::dataset_dependency>& dependencies) {
    datasetDependencies_ = dependencies;
}

void DatasetViewDialog::setDatasetNames(const std::map<std::string, std::string>& codeToName) {
    datasetNames_ = codeToName;
}

void DatasetViewDialog::updateOverviewTab() {
    overviewDescriptionLabel_->setText(
        dataset_.description.empty() ? tr("-") : QString::fromStdString(dataset_.description));
    overviewCommentaryLabel_->setText(
        dataset_.change_commentary.empty() ?
            tr("-") :
            QString::fromStdString(dataset_.change_commentary));

    overviewDomainLabel_->setText(QString::fromStdString(dataset_.domain_name));
    overviewSubjectAreaLabel_->setText(QString::fromStdString(dataset_.subject_area_name));
    overviewCatalogLabel_->setText(
        dataset_.catalog_name ? QString::fromStdString(*dataset_.catalog_name) : tr("-"));

    // Same badge domains as DatasetItemDelegate's list-row rendering.
    BadgeLabelUtils::apply(overviewOriginBadge_,
                          badgeCache_,
                          "dq_origin",
                          dataset_.origin_code,
                          QString::fromStdString(dataset_.origin_code));
    BadgeLabelUtils::apply(overviewNatureBadge_,
                          badgeCache_,
                          "dq_nature",
                          dataset_.nature_code,
                          QString::fromStdString(dataset_.nature_code));
    BadgeLabelUtils::apply(overviewTreatmentBadge_,
                          badgeCache_,
                          "dq_treatment",
                          dataset_.treatment_code,
                          QString::fromStdString(dataset_.treatment_code));

    overviewModifiedByLabel_->setText(QString::fromStdString(dataset_.modified_by));
    overviewRecordedAtLabel_->setText(relative_time_helper::format(dataset_.recorded_at));
}

void DatasetViewDialog::updateProvenanceAndMethodologyTab() {
    businessContextLabel_->setText(
        dataset_.business_context.empty() ?
            tr("-") :
            QString::fromStdString(dataset_.business_context));

    asOfDateLabel_->setText(relative_time_helper::format(dataset_.as_of_date));
    ingestionLabel_->setText(relative_time_helper::format(dataset_.ingestion_timestamp));

    sourceSystemLabel_->setText(QString::fromStdString(dataset_.source_system_id));
    licenseLabel_->setText(
        dataset_.license_info ? QString::fromStdString(*dataset_.license_info) : tr("-"));
    codingSchemeLabel_->setText(
        dataset_.coding_scheme_code ? QString::fromStdString(*dataset_.coding_scheme_code) :
                                      tr("-"));

    upstreamDerivationLabel_->setText(
        dataset_.upstream_derivation_id ?
            QString::fromStdString(boost::uuids::to_string(*dataset_.upstream_derivation_id)) :
            tr("-"));
    lineageDepthLabel_->setText(QString::number(dataset_.lineage_depth));

    stepsText_->clear();
    const auto* methodology = findMethodology(dataset_.methodology_id);
    if (!methodology) {
        methodologyDescriptionLabel_->setText(tr("No methodology assigned."));
        commentaryLabel_->setText(tr("-"));
        methodologyNameLabel_->setText(tr("-"));
        methodologyVersionLabel_->setText(tr("-"));
        methodologyIdLabel_->setText(tr("-"));
        logicReferenceLabel_->setText(tr("-"));
        pmModifiedByLabel_->setText(tr("-"));
        pmRecordedAtLabel_->setText(tr("-"));
        stepsText_->setPlainText(tr("No implementation details available."));
        return;
    }

    methodologyDescriptionLabel_->setText(
        methodology->description.empty() ? tr("-") :
                                           QString::fromStdString(methodology->description));
    commentaryLabel_->setText(
        methodology->change_commentary.empty() ?
            tr("-") :
            QString::fromStdString(methodology->change_commentary));
    methodologyNameLabel_->setText(QString::fromStdString(methodology->name));
    methodologyVersionLabel_->setText(QString::number(methodology->version));
    methodologyIdLabel_->setText(
        QString::fromStdString(boost::uuids::to_string(methodology->id)));
    logicReferenceLabel_->setText(
        methodology->logic_reference ? QString::fromStdString(*methodology->logic_reference) :
                                       tr("-"));
    pmModifiedByLabel_->setText(QString::fromStdString(methodology->modified_by));
    pmRecordedAtLabel_->setText(relative_time_helper::format(methodology->recorded_at));

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
    const qreal rowSpacing = 50; // Vertical spacing between rows
    qreal currentX = 0;
    qreal dependencyRow = 0;
    qreal datasetRow =
        rowSpacing + lineageHeaderHeight_ + lineageRowHeight_ * 3; // 3 rows: header + Code + Name

    // Row 1: Dependency datasets (datasets this dataset depends on)
    struct DepInfo {
        std::string code;
        std::string name;
        std::string role;
        qreal nodeX;
        qreal nodeHeight;
    };
    std::vector<DepInfo> dependencies;

    if (!datasetDependencies_.empty()) {
        // Find dependencies where this dataset depends on others
        for (const auto& dep : datasetDependencies_) {
            if (dep.dataset_code == dataset_.code) {
                std::string name;
                auto it = datasetNames_.find(dep.dependency_code);
                if (it != datasetNames_.end()) {
                    name = it->second;
                }
                dependencies.push_back({dep.dependency_code, name, dep.role, 0, 0});
            }
        }

        // Create nodes for dependency datasets (shown as Dataset nodes, not Dependency)
        for (auto& dep : dependencies) {
            dep.nodeX = currentX;
            dep.nodeHeight = createLineageNode(
                scene,
                currentX,
                dependencyRow,
                tr("Dataset"),
                {tr("Code"), tr("Name")},
                {QString::fromStdString(dep.code), QString::fromStdString(dep.name)},
                lineageHeaderCatalog_,
                false,
                true);

            currentX += lineageNodeWidth_ + lineageNodeSpacing_;
        }
    }

    // Row 2: Current dataset node
    qreal datasetX = 0;
    if (!dependencies.empty()) {
        // Center dataset under the dependency nodes
        qreal totalWidth = currentX - lineageNodeSpacing_;
        datasetX = (totalWidth - lineageNodeWidth_) / 2;
    }

    qreal datasetHeight = createLineageNode(
        scene,
        datasetX,
        datasetRow,
        tr("Dataset"),
        {tr("Code"), tr("Name")},
        {QString::fromStdString(dataset_.code), QString::fromStdString(dataset_.name)},
        lineageHeaderDataset_,
        !dependencies.empty(),
        false);

    // Draw labeled connections from dependencies to dataset
    if (!dependencies.empty()) {
        qreal datasetCenterX = datasetX + lineageNodeWidth_ / 2;
        qreal datasetTopY = datasetRow - lineageSocketRadius_;
        for (const auto& dep : dependencies) {
            qreal depRightX = dep.nodeX + lineageNodeWidth_ + lineageSocketRadius_;
            qreal depCenterY = dependencyRow + dep.nodeHeight / 2;
            drawLabeledConnection(scene,
                                  depRightX,
                                  depCenterY,
                                  datasetCenterX,
                                  datasetTopY,
                                  QString::fromStdString(dep.role));
        }
    }

    // Set scene background
    scene->setBackgroundBrush(QBrush(lineageBackground_));

    // Fit view with some margin
    QRectF sceneRect = scene->itemsBoundingRect();
    sceneRect.adjust(-16, -16, 16, 16);
    lineageView_->setFitRect(sceneRect);
    lineageView_->resetTransform();
    lineageView_->fitInView(sceneRect, Qt::KeepAspectRatio);
}

qreal DatasetViewDialog::createLineageNode(QGraphicsScene* scene,
                                           qreal x,
                                           qreal y,
                                           const QString& headerText,
                                           const QStringList& labels,
                                           const QStringList& values,
                                           const QColor& headerColor,
                                           bool hasInputSocket,
                                           bool hasOutputSocket) const {

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
    createLineageNodeProperties(
        scene, x, y + lineageHeaderHeight_ + lineagePadding_, labels, values);
    createLineageNodeSockets(scene, x, y, nodeHeight, hasInputSocket, hasOutputSocket);

    return nodeHeight;
}

void DatasetViewDialog::createLineageNodeBody(
    QGraphicsScene* scene, qreal x, qreal y, qreal nodeHeight, const QString& tooltip) const {

    QPainterPath bodyPath;
    bodyPath.addRoundedRect(
        x, y, lineageNodeWidth_, nodeHeight, lineageCornerRadius_, lineageCornerRadius_);
    auto* bodyItem = scene->addPath(bodyPath, QPen(lineageNodeBorder_), QBrush(lineageNodeBody_));
    bodyItem->setToolTip(tooltip);
}

void DatasetViewDialog::createLineageNodeHeader(QGraphicsScene* scene,
                                                qreal x,
                                                qreal y,
                                                const QString& headerText,
                                                const QColor& headerColor,
                                                const QString& tooltip) const {

    // Header background (top rounded, bottom square)
    QPainterPath headerPath;
    headerPath.moveTo(x + lineageCornerRadius_, y);
    headerPath.arcTo(x, y, lineageCornerRadius_ * 2, lineageCornerRadius_ * 2, 90, 90);
    headerPath.lineTo(x, y + lineageHeaderHeight_);
    headerPath.lineTo(x + lineageNodeWidth_, y + lineageHeaderHeight_);
    headerPath.lineTo(x + lineageNodeWidth_, y + lineageCornerRadius_);
    headerPath.arcTo(x + lineageNodeWidth_ - lineageCornerRadius_ * 2,
                     y,
                     lineageCornerRadius_ * 2,
                     lineageCornerRadius_ * 2,
                     0,
                     90);
    headerPath.closeSubpath();
    auto* headerBg = scene->addPath(headerPath, QPen(Qt::NoPen), QBrush(headerColor));
    headerBg->setToolTip(tooltip);

    // Header text (centered)
    QFont headerFont;
    headerFont.setPointSize(6);
    headerFont.setBold(true);
    QFontMetrics headerFm(headerFont);

    QString elidedHeader = headerFm.elidedText(
        headerText, Qt::ElideRight, static_cast<int>(lineageNodeWidth_ - 2 * lineagePadding_));
    auto* headerItem = scene->addText(elidedHeader);
    headerItem->setFont(headerFont);
    headerItem->setDefaultTextColor(lineageText_);
    headerItem->setToolTip(tooltip);
    qreal headerTextWidth = headerFm.horizontalAdvance(elidedHeader);
    headerItem->setPos(x + (lineageNodeWidth_ - headerTextWidth) / 2,
                       y + (lineageHeaderHeight_ - headerFm.height()) / 2 - 1);
}

void DatasetViewDialog::createLineageNodeProperties(QGraphicsScene* scene,
                                                    qreal x,
                                                    qreal y,
                                                    const QStringList& labels,
                                                    const QStringList& values) const {

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

void DatasetViewDialog::createLineageNodeSockets(QGraphicsScene* scene,
                                                 qreal x,
                                                 qreal y,
                                                 qreal nodeHeight,
                                                 bool hasInput,
                                                 bool hasOutput) const {

    qreal socketY = y + nodeHeight / 2;

    if (hasInput) {
        scene->addEllipse(x - lineageSocketRadius_,
                          socketY - lineageSocketRadius_,
                          lineageSocketRadius_ * 2,
                          lineageSocketRadius_ * 2,
                          QPen(lineageNodeBorder_),
                          QBrush(lineageSocket_));
    }

    if (hasOutput) {
        scene->addEllipse(x + lineageNodeWidth_ - lineageSocketRadius_,
                          socketY - lineageSocketRadius_,
                          lineageSocketRadius_ * 2,
                          lineageSocketRadius_ * 2,
                          QPen(lineageNodeBorder_),
                          QBrush(lineageSocket_));
    }
}

void DatasetViewDialog::drawLineageConnection(
    QGraphicsScene* scene, qreal x1, qreal y1, qreal x2, qreal y2) const {

    QPainterPath path;
    path.moveTo(x1, y1);
    qreal ctrlOffset = (x2 - x1) * 0.4;
    path.cubicTo(x1 + ctrlOffset, y1, x2 - ctrlOffset, y2, x2, y2);
    scene->addPath(path, QPen(lineageConnection_, 1.2));
}

void DatasetViewDialog::drawLabeledConnection(
    QGraphicsScene* scene, qreal x1, qreal y1, qreal x2, qreal y2, const QString& label) const {

    // Draw the connection line
    QPainterPath path;
    path.moveTo(x1, y1);
    qreal ctrlOffset = std::abs(x2 - x1) * 0.4;
    qreal ctrlOffsetY = std::abs(y2 - y1) * 0.4;
    path.cubicTo(x1 + ctrlOffset, y1, x2, y2 - ctrlOffsetY, x2, y2);
    scene->addPath(path, QPen(lineageConnection_, 1.5));

    // Add label at midpoint of the connection
    qreal midX = (x1 + x2) / 2;
    qreal midY = (y1 + y2) / 2;

    QFont labelFont;
    labelFont.setPointSize(5);
    labelFont.setItalic(true);
    QFontMetrics fm(labelFont);

    // Create background for label
    qreal labelWidth = fm.horizontalAdvance(label) + 4;
    qreal labelHeight = fm.height() + 2;
    scene->addRect(midX - labelWidth / 2,
                   midY - labelHeight / 2,
                   labelWidth,
                   labelHeight,
                   QPen(Qt::NoPen),
                   QBrush(lineageBackground_));

    // Add label text
    auto* textItem = scene->addText(label);
    textItem->setFont(labelFont);
    textItem->setDefaultTextColor(lineageLabel_);
    textItem->setPos(midX - fm.horizontalAdvance(label) / 2, midY - fm.height() / 2);
}

}
