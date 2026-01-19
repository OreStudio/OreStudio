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
#include "ores.qt/DataLibrarianWindow.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QGroupBox>
#include <QFormLayout>
#include <QGraphicsScene>
#include <QGraphicsRectItem>
#include <QGraphicsTextItem>
#include <QGraphicsLineItem>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

// Navigation tree item roles
constexpr int ItemTypeRole = Qt::UserRole + 1;
constexpr int ItemIdRole = Qt::UserRole + 2;

enum class NavigationItemType {
    Root,
    Domain,
    SubjectArea,
    Catalog
};

}

DataLibrarianWindow::DataLibrarianWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      mainSplitter_(new QSplitter(Qt::Horizontal, this)),
      centralSplitter_(new QSplitter(Qt::Vertical, this)),
      navigationTree_(new QTreeView(this)),
      navigationModel_(new QStandardItemModel(this)),
      toolbar_(new QToolBar(this)),
      datasetTable_(new QTableView(this)),
      datasetModel_(new ClientDatasetModel(clientManager, this)),
      datasetProxyModel_(new QSortFilterProxyModel(this)),
      detailPanel_(new QWidget(this)),
      dataDomainModel_(new ClientDataDomainModel(clientManager, this)),
      subjectAreaModel_(new ClientSubjectAreaModel(clientManager, this)),
      catalogModel_(new ClientCatalogModel(clientManager, this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating Data Librarian window";

    setupUi();
    setupToolbar();
    setupNavigationSidebar();
    setupCentralWorkspace();
    setupDetailPanel();
    setupConnections();

    // Load data
    emit statusChanged(tr("Loading data..."));
    dataDomainModel_->refresh();
    subjectAreaModel_->refresh();
    catalogModel_->loadData();
    datasetModel_->refresh();
}

void DataLibrarianWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    mainLayout->addWidget(toolbar_);
    mainLayout->addWidget(mainSplitter_, 1);

    // Configure main splitter: sidebar | central
    mainSplitter_->setHandleWidth(1);
    mainSplitter_->setChildrenCollapsible(false);

    // Configure central splitter: table | detail
    centralSplitter_->setHandleWidth(1);
    centralSplitter_->setChildrenCollapsible(false);
}

void DataLibrarianWindow::setupToolbar() {
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_arrow_sync_20_regular.svg", iconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh all data"));

    toolbar_->addSeparator();

    // Related windows section
    auto* relatedLabel = new QLabel(tr("  Open:  "), toolbar_);
    toolbar_->addWidget(relatedLabel);

    originDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_database_20_regular.svg", iconColor),
        tr("Origin"));
    originDimensionsAction_->setToolTip(tr("Open Origin Dimensions window"));

    natureDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_database_20_regular.svg", iconColor),
        tr("Nature"));
    natureDimensionsAction_->setToolTip(tr("Open Nature Dimensions window"));

    treatmentDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_database_20_regular.svg", iconColor),
        tr("Treatment"));
    treatmentDimensionsAction_->setToolTip(tr("Open Treatment Dimensions window"));

    toolbar_->addSeparator();

    codingSchemesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_code_20_regular.svg", iconColor),
        tr("Schemes"));
    codingSchemesAction_->setToolTip(tr("Open Coding Schemes window"));

    methodologiesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_book_20_regular.svg", iconColor),
        tr("Methods"));
    methodologiesAction_->setToolTip(tr("Open Methodologies window"));
}

void DataLibrarianWindow::setupNavigationSidebar() {
    // Configure tree view
    navigationTree_->setModel(navigationModel_);
    navigationTree_->setHeaderHidden(true);
    navigationTree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    navigationTree_->setMinimumWidth(200);
    navigationTree_->setMaximumWidth(350);

    // Add to main splitter
    mainSplitter_->addWidget(navigationTree_);

    // Initialize navigation model with root items
    navigationModel_->setHorizontalHeaderLabels({tr("Navigation")});

    auto* rootItem = navigationModel_->invisibleRootItem();

    // Add "All Datasets" item
    auto* allDatasetsItem = new QStandardItem(tr("All Datasets"));
    allDatasetsItem->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
    allDatasetsItem->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", color_constants::icon_color));
    rootItem->appendRow(allDatasetsItem);

    // Add "Domains" parent item
    auto* domainsItem = new QStandardItem(tr("Domains"));
    domainsItem->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
    domainsItem->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", color_constants::icon_color));
    rootItem->appendRow(domainsItem);
}

void DataLibrarianWindow::setupCentralWorkspace() {
    // Configure dataset table
    datasetProxyModel_->setSourceModel(datasetModel_);
    datasetProxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    datasetTable_->setModel(datasetProxyModel_);
    datasetTable_->setSortingEnabled(true);
    datasetTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    datasetTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    datasetTable_->setAlternatingRowColors(true);
    datasetTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    datasetTable_->horizontalHeader()->setStretchLastSection(true);
    datasetTable_->verticalHeader()->setVisible(false);
    datasetTable_->sortByColumn(ClientDatasetModel::Name, Qt::AscendingOrder);

    // Add table to central splitter
    centralSplitter_->addWidget(datasetTable_);

    // Add central splitter to main splitter
    mainSplitter_->addWidget(centralSplitter_);

    // Set splitter sizes: sidebar 250px, central takes the rest
    mainSplitter_->setSizes({250, 1150});
}

void DataLibrarianWindow::setupDetailPanel() {
    auto* detailLayout = new QVBoxLayout(detailPanel_);
    detailLayout->setContentsMargins(8, 8, 8, 8);

    // Header
    auto* headerLabel = new QLabel(tr("<b>Dataset Accession Card</b>"), detailPanel_);
    detailLayout->addWidget(headerLabel);

    // Main content in horizontal layout
    auto* contentLayout = new QHBoxLayout();

    // Left side: Classification and basic info
    auto* classificationGroup = new QGroupBox(tr("Classification"), detailPanel_);
    auto* classificationLayout = new QFormLayout(classificationGroup);

    datasetNameLabel_ = new QLabel(tr("-"), detailPanel_);
    datasetNameLabel_->setWordWrap(true);
    classificationLayout->addRow(tr("Name:"), datasetNameLabel_);

    datasetUuidLabel_ = new QLabel(tr("-"), detailPanel_);
    datasetUuidLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    classificationLayout->addRow(tr("UUID:"), datasetUuidLabel_);

    catalogLabel_ = new QLabel(tr("-"), detailPanel_);
    classificationLayout->addRow(tr("Catalog:"), catalogLabel_);

    classificationLayout->addRow(new QLabel(tr("")));

    originLabel_ = new QLabel(tr("-"), detailPanel_);
    classificationLayout->addRow(tr("Origin:"), originLabel_);

    natureLabel_ = new QLabel(tr("-"), detailPanel_);
    classificationLayout->addRow(tr("Nature:"), natureLabel_);

    treatmentLabel_ = new QLabel(tr("-"), detailPanel_);
    classificationLayout->addRow(tr("Treatment:"), treatmentLabel_);

    contentLayout->addWidget(classificationGroup);

    // Center: Lineage & Methodology
    auto* lineageGroup = new QGroupBox(tr("The Trace (Lineage)"), detailPanel_);
    auto* lineageLayout = new QVBoxLayout(lineageGroup);

    methodologyLabel_ = new QLabel(tr("Methodology: -"), detailPanel_);
    lineageLayout->addWidget(methodologyLabel_);

    descriptionLabel_ = new QLabel(tr(""), detailPanel_);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setStyleSheet("color: #888; font-style: italic;");
    lineageLayout->addWidget(descriptionLabel_);

    // Lineage visualization
    lineageView_ = new QGraphicsView(detailPanel_);
    lineageView_->setMinimumHeight(100);
    lineageView_->setMaximumHeight(150);
    lineageView_->setScene(new QGraphicsScene(lineageView_));
    lineageView_->setRenderHint(QPainter::Antialiasing);
    lineageLayout->addWidget(lineageView_);

    contentLayout->addWidget(lineageGroup, 1);

    detailLayout->addLayout(contentLayout);

    // Add detail panel to central splitter
    centralSplitter_->addWidget(detailPanel_);

    // Set central splitter sizes: table 60%, detail 40%
    centralSplitter_->setSizes({400, 300});

    // Initially hide detail panel until a dataset is selected
    detailPanel_->setVisible(false);
}

void DataLibrarianWindow::setupConnections() {
    // Navigation tree selection
    connect(navigationTree_->selectionModel(),
            &QItemSelectionModel::currentChanged,
            this, &DataLibrarianWindow::onNavigationSelectionChanged);

    // Dataset table selection
    connect(datasetTable_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &DataLibrarianWindow::onDatasetSelectionChanged);

    connect(datasetTable_, &QTableView::doubleClicked,
            this, &DataLibrarianWindow::onDatasetDoubleClicked);

    // Toolbar actions
    connect(refreshAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onRefreshClicked);

    connect(originDimensionsAction_, &QAction::triggered,
            this, &DataLibrarianWindow::openOriginDimensionsRequested);
    connect(natureDimensionsAction_, &QAction::triggered,
            this, &DataLibrarianWindow::openNatureDimensionsRequested);
    connect(treatmentDimensionsAction_, &QAction::triggered,
            this, &DataLibrarianWindow::openTreatmentDimensionsRequested);
    connect(codingSchemesAction_, &QAction::triggered,
            this, &DataLibrarianWindow::openCodingSchemesRequested);
    connect(methodologiesAction_, &QAction::triggered,
            this, &DataLibrarianWindow::openMethodologiesRequested);

    // Data model signals
    connect(datasetModel_, &ClientDatasetModel::dataLoaded,
            this, &DataLibrarianWindow::onDataLoaded);
    connect(datasetModel_, &ClientDatasetModel::loadError,
            this, &DataLibrarianWindow::onLoadError);

    connect(dataDomainModel_, &ClientDataDomainModel::dataLoaded,
            this, &DataLibrarianWindow::onDomainsLoaded);
    connect(subjectAreaModel_, &ClientSubjectAreaModel::dataLoaded,
            this, &DataLibrarianWindow::onSubjectAreasLoaded);
    connect(catalogModel_, &ClientCatalogModel::loadFinished,
            this, &DataLibrarianWindow::onCatalogsLoaded);
}

void DataLibrarianWindow::onNavigationSelectionChanged(
    const QModelIndex& current, const QModelIndex& /*previous*/) {

    if (!current.isValid()) {
        clearDatasetFilter();
        return;
    }

    const auto itemType = static_cast<NavigationItemType>(
        current.data(ItemTypeRole).toInt());
    const auto itemName = current.data(ItemIdRole).toString();

    switch (itemType) {
    case NavigationItemType::Root:
        clearDatasetFilter();
        break;
    case NavigationItemType::Catalog: {
        if (!itemName.isEmpty()) {
            filterDatasetsByCatalog(itemName);
        }
        break;
    }
    default:
        // For Domain and SubjectArea, show all datasets under them
        // TODO: Implement hierarchical filtering by domain_name/subject_area_name
        clearDatasetFilter();
        break;
    }
}

void DataLibrarianWindow::onDatasetSelectionChanged() {
    const auto selected = datasetTable_->selectionModel()->selectedRows();

    if (selected.isEmpty()) {
        detailPanel_->setVisible(false);
        return;
    }

    const auto sourceIndex = datasetProxyModel_->mapToSource(selected.first());
    const auto* dataset = datasetModel_->getDataset(sourceIndex.row());

    updateDetailPanel(dataset);
    detailPanel_->setVisible(true);
}

void DataLibrarianWindow::onDatasetDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    const auto sourceIndex = datasetProxyModel_->mapToSource(index);
    const auto* dataset = datasetModel_->getDataset(sourceIndex.row());

    if (dataset) {
        emit showDatasetDetails(*dataset);
    }
}

void DataLibrarianWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing data..."));
    dataDomainModel_->refresh();
    subjectAreaModel_->refresh();
    catalogModel_->loadData();
    datasetModel_->refresh();
}

void DataLibrarianWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 datasets").arg(datasetModel_->rowCount()));
}

void DataLibrarianWindow::onLoadError(const QString& error_message,
                                       const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void DataLibrarianWindow::onDomainsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "Domains loaded: "
                               << dataDomainModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::onSubjectAreasLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "Subject areas loaded: "
                               << subjectAreaModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::onCatalogsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "Catalogs loaded: "
                               << catalogModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::buildNavigationTree() {
    // Only build when some data is loaded
    if (dataDomainModel_->rowCount() == 0 &&
        catalogModel_->rowCount() == 0) {
        return;
    }

    auto* rootItem = navigationModel_->invisibleRootItem();

    // Find or create parent items
    QStandardItem* domainsParent = nullptr;
    QStandardItem* catalogsParent = nullptr;

    for (int i = 0; i < rootItem->rowCount(); ++i) {
        auto* item = rootItem->child(i);
        if (item->text() == tr("Domains")) {
            domainsParent = item;
        } else if (item->text() == tr("Catalogs")) {
            catalogsParent = item;
        }
    }

    // Create Catalogs parent if not exists
    if (!catalogsParent) {
        catalogsParent = new QStandardItem(tr("Catalogs"));
        catalogsParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        catalogsParent->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_library_20_regular.svg", color_constants::icon_color));
        rootItem->appendRow(catalogsParent);
    }

    // Build Domains hierarchy using string-based relationships
    if (domainsParent) {
        domainsParent->removeRows(0, domainsParent->rowCount());

        for (int d = 0; d < dataDomainModel_->rowCount(); ++d) {
            const auto* domain = dataDomainModel_->getDomain(d);
            if (!domain) continue;

            auto* domainItem = new QStandardItem(
                QString::fromStdString(domain->name));
            domainItem->setData(static_cast<int>(NavigationItemType::Domain),
                               ItemTypeRole);
            // Store name as string for filtering
            domainItem->setData(QString::fromStdString(domain->name), ItemIdRole);
            domainItem->setIcon(IconUtils::createRecoloredIcon(
                ":/icons/ic_fluent_folder_20_regular.svg", color_constants::icon_color));

            // Find subject areas for this domain (using domain_name string FK)
            for (int s = 0; s < subjectAreaModel_->rowCount(); ++s) {
                const auto* subjectArea = subjectAreaModel_->getSubjectArea(s);
                if (!subjectArea || subjectArea->domain_name != domain->name) {
                    continue;
                }

                auto* subjectAreaItem = new QStandardItem(
                    QString::fromStdString(subjectArea->name));
                subjectAreaItem->setData(
                    static_cast<int>(NavigationItemType::SubjectArea), ItemTypeRole);
                subjectAreaItem->setData(
                    QString::fromStdString(subjectArea->name), ItemIdRole);
                subjectAreaItem->setIcon(IconUtils::createRecoloredIcon(
                    ":/icons/ic_fluent_table_20_regular.svg", color_constants::icon_color));

                domainItem->appendRow(subjectAreaItem);
            }

            domainsParent->appendRow(domainItem);
        }

        navigationTree_->expand(domainsParent->index());
    }

    // Build Catalogs list (catalogs are independent, no hierarchy)
    if (catalogsParent) {
        catalogsParent->removeRows(0, catalogsParent->rowCount());

        for (int c = 0; c < catalogModel_->rowCount(); ++c) {
            const auto& catalog = catalogModel_->catalogAt(c);

            auto* catalogItem = new QStandardItem(
                QString::fromStdString(catalog.name));
            catalogItem->setData(
                static_cast<int>(NavigationItemType::Catalog), ItemTypeRole);
            catalogItem->setData(
                QString::fromStdString(catalog.name), ItemIdRole);
            catalogItem->setIcon(IconUtils::createRecoloredIcon(
                ":/icons/ic_fluent_library_20_regular.svg", color_constants::icon_color));

            catalogsParent->appendRow(catalogItem);
        }
    }
}

void DataLibrarianWindow::updateDetailPanel(const dq::domain::dataset* dataset) {
    if (!dataset) {
        datasetNameLabel_->setText(tr("-"));
        datasetUuidLabel_->setText(tr("-"));
        catalogLabel_->setText(tr("-"));
        originLabel_->setText(tr("-"));
        natureLabel_->setText(tr("-"));
        treatmentLabel_->setText(tr("-"));
        methodologyLabel_->setText(tr("Methodology: -"));
        descriptionLabel_->setText(tr(""));
        return;
    }

    datasetNameLabel_->setText(QString::fromStdString(dataset->name));
    datasetUuidLabel_->setText(
        QString::fromStdString(boost::uuids::to_string(dataset->id)));
    catalogLabel_->setText(dataset->catalog_name
        ? QString::fromStdString(*dataset->catalog_name)
        : tr("-"));

    // Dimensions (stored as codes, not UUIDs)
    originLabel_->setText(QString::fromStdString(dataset->origin_code));
    natureLabel_->setText(QString::fromStdString(dataset->nature_code));
    treatmentLabel_->setText(QString::fromStdString(dataset->treatment_code));

    // Methodology
    methodologyLabel_->setText(
        tr("Methodology: %1").arg(findMethodologyName(dataset->methodology_id)));

    // Description
    if (!dataset->description.empty()) {
        descriptionLabel_->setText(
            QString("\"%1\"").arg(QString::fromStdString(dataset->description)));
    } else {
        descriptionLabel_->setText(tr(""));
    }

    updateLineageView(dataset);
}

void DataLibrarianWindow::updateLineageView(const dq::domain::dataset* dataset) {
    auto* scene = lineageView_->scene();
    scene->clear();

    if (!dataset) {
        return;
    }

    // Simple lineage flow: Source -> Methodology -> Dataset
    const QColor boxColor(60, 60, 80);
    const QColor textColor(220, 220, 220);
    const QColor lineColor(100, 150, 200);

    const qreal boxWidth = 120;
    const qreal boxHeight = 50;
    const qreal spacing = 80;

    // Source box (Origin dimension)
    auto* sourceRect = scene->addRect(0, 0, boxWidth, boxHeight,
                                       QPen(lineColor), QBrush(boxColor));
    auto* sourceText = scene->addText(QString::fromStdString(dataset->origin_code));
    sourceText->setDefaultTextColor(textColor);
    sourceText->setPos(10, 15);

    // Methodology box
    auto* methodRect = scene->addRect(boxWidth + spacing, 0, boxWidth, boxHeight,
                                       QPen(lineColor), QBrush(boxColor));
    auto* methodText = scene->addText(findMethodologyName(dataset->methodology_id));
    methodText->setDefaultTextColor(textColor);
    methodText->setPos(boxWidth + spacing + 10, 15);

    // Dataset box
    auto* datasetRect = scene->addRect(2 * (boxWidth + spacing), 0,
                                        boxWidth, boxHeight,
                                        QPen(lineColor), QBrush(boxColor));
    auto* datasetText = scene->addText(
        QString::fromStdString(dataset->name).left(15));
    datasetText->setDefaultTextColor(textColor);
    datasetText->setPos(2 * (boxWidth + spacing) + 10, 15);

    // Arrows
    QPen arrowPen(lineColor, 2);

    // Arrow 1: Source -> Methodology
    scene->addLine(boxWidth, boxHeight / 2,
                   boxWidth + spacing, boxHeight / 2, arrowPen);

    // Arrow 2: Methodology -> Dataset
    scene->addLine(2 * boxWidth + spacing, boxHeight / 2,
                   2 * boxWidth + 2 * spacing, boxHeight / 2, arrowPen);

    lineageView_->fitInView(scene->sceneRect(), Qt::KeepAspectRatio);
}

void DataLibrarianWindow::filterDatasetsByCatalog(const QString& catalogName) {
    selectedCatalogName_ = catalogName;
    // TODO: Implement actual filtering on the proxy model
    // For now, just log the filter request
    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by catalog: "
                               << catalogName.toStdString();
}

void DataLibrarianWindow::clearDatasetFilter() {
    selectedCatalogName_.clear();
    datasetProxyModel_->setFilterRegularExpression("");
}

QString DataLibrarianWindow::findOriginDimensionName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores origin_code directly
    return tr("N/A");
}

QString DataLibrarianWindow::findNatureDimensionName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores nature_code directly
    return tr("N/A");
}

QString DataLibrarianWindow::findTreatmentDimensionName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores treatment_code directly
    return tr("N/A");
}

QString DataLibrarianWindow::findMethodologyName(
    const std::optional<boost::uuids::uuid>& id) const {
    if (!id) {
        return tr("None");
    }
    // TODO: Look up from a loaded methodologies cache
    return QString::fromStdString(boost::uuids::to_string(*id)).left(8) + "...";
}

QString DataLibrarianWindow::findCatalogName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores catalog_name directly
    return tr("N/A");
}

}
