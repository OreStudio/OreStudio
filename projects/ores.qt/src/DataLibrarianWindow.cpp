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
#include <QMenu>
#include <QSettings>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/DatasetItemDelegate.hpp"

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

Icon iconForSubjectArea(const QString& subjectAreaName) {
    if (subjectAreaName == "Countries") {
        return Icon::Globe;
    } else if (subjectAreaName == "Currencies") {
        return Icon::Currency;
    } else if (subjectAreaName == "Cryptocurrencies") {
        return Icon::Currency;
    }
    return Icon::Table;
}

}

DataLibrarianWindow::DataLibrarianWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      mainSplitter_(new QSplitter(Qt::Horizontal, this)),
      navigationTree_(new QTreeView(this)),
      navigationModel_(new QStandardItemModel(this)),
      toolbar_(new QToolBar(this)),
      datasetTable_(new QTableView(this)),
      datasetModel_(new ClientDatasetModel(clientManager, this)),
      datasetProxyModel_(new QSortFilterProxyModel(this)),
      dataDomainModel_(new ClientDataDomainModel(clientManager, this)),
      subjectAreaModel_(new ClientSubjectAreaModel(clientManager, this)),
      catalogModel_(new ClientCatalogModel(clientManager, this)),
      methodologyModel_(new ClientMethodologyModel(clientManager, this)),
      statusBar_(new QStatusBar(this)),
      loadingProgressBar_(new QProgressBar(this)),
      statusLabel_(new QLabel(this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating Data Librarian window";

    setupUi();
    setupToolbar();
    setupNavigationSidebar();
    setupCentralWorkspace();
    setupConnections();
    setupColumnVisibility();

    // Configure loading progress bar
    totalLoads_ = total_model_loads;
    pendingLoads_ = totalLoads_;
    loadingProgressBar_->setRange(0, totalLoads_);
    loadingProgressBar_->setValue(0);
    loadingProgressBar_->setTextVisible(false);
    loadingProgressBar_->setFixedWidth(120);
    loadingProgressBar_->setFixedHeight(16);

    // Load data with detailed status
    BOOST_LOG_SEV(lg(), info) << "Starting data load for Data Librarian window";

    statusLabel_->setText(tr("Loading domains..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting domains...";
    dataDomainModel_->refresh();

    statusLabel_->setText(tr("Loading subject areas..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting subject areas...";
    subjectAreaModel_->refresh();

    statusLabel_->setText(tr("Loading catalogs..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting catalogs...";
    catalogModel_->loadData();

    statusLabel_->setText(tr("Loading methodologies..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting methodologies...";
    methodologyModel_->refresh();

    statusLabel_->setText(tr("Loading datasets..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting datasets...";
    datasetModel_->refresh();
}

void DataLibrarianWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    mainLayout->addWidget(toolbar_);
    mainLayout->addWidget(mainSplitter_, 1);

    // Configure status bar
    statusBar_->addWidget(statusLabel_, 1);
    statusBar_->addPermanentWidget(loadingProgressBar_);
    statusBar_->setSizeGripEnabled(false);
    statusBar_->setMinimumHeight(22);
    statusBar_->setContentsMargins(4, 0, 4, 2);
    mainLayout->addWidget(statusBar_);

    // Configure main splitter: sidebar | dataset table
    mainSplitter_->setHandleWidth(1);
    mainSplitter_->setChildrenCollapsible(false);
}

void DataLibrarianWindow::setupToolbar() {
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh all data"));

    viewDatasetAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Info, IconUtils::DefaultIconColor),
        tr("View"));
    viewDatasetAction_->setToolTip(tr("View selected dataset details"));
    viewDatasetAction_->setEnabled(false);

    toolbar_->addSeparator();

    // Related windows - dimensions
    originDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Database, IconUtils::DefaultIconColor),
        tr("Origin"));
    originDimensionsAction_->setToolTip(tr("Open Origin Dimensions window"));

    natureDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Database, IconUtils::DefaultIconColor),
        tr("Nature"));
    natureDimensionsAction_->setToolTip(tr("Open Nature Dimensions window"));

    treatmentDimensionsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Database, IconUtils::DefaultIconColor),
        tr("Treatment"));
    treatmentDimensionsAction_->setToolTip(tr("Open Treatment Dimensions window"));

    toolbar_->addSeparator();

    codingSchemesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Code, IconUtils::DefaultIconColor),
        tr("Schemes"));
    codingSchemesAction_->setToolTip(tr("Open Coding Schemes window"));

    methodologiesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Book, IconUtils::DefaultIconColor),
        tr("Methods"));
    methodologiesAction_->setToolTip(tr("Open Methodologies window"));
}

void DataLibrarianWindow::setupNavigationSidebar() {
    // Create left panel container for navigation tree
    auto* leftPanel = new QWidget(this);
    auto* leftLayout = new QVBoxLayout(leftPanel);
    leftLayout->setContentsMargins(0, 0, 0, 0);
    leftLayout->setSpacing(8);

    // Dataset Browser section
    auto* browserLabel = new QLabel(tr("<b>Dataset Browser</b>"), leftPanel);
    leftLayout->addWidget(browserLabel);

    // Configure tree view
    navigationTree_->setModel(navigationModel_);
    navigationTree_->setHeaderHidden(true);
    navigationTree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    navigationTree_->setFrameShape(QFrame::StyledPanel);
    navigationTree_->setFrameShadow(QFrame::Sunken);
    leftLayout->addWidget(navigationTree_, 1);

    // Set minimum/maximum width for left panel
    leftPanel->setMinimumWidth(250);
    leftPanel->setMaximumWidth(400);

    // Add to main splitter
    mainSplitter_->addWidget(leftPanel);

    // Initialize navigation model with root items
    navigationModel_->setHorizontalHeaderLabels({tr("Navigation")});

    auto* rootItem = navigationModel_->invisibleRootItem();

    // Add "All Datasets" item
    auto* allDatasetsItem = new QStandardItem(tr("All Datasets"));
    allDatasetsItem->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
    allDatasetsItem->setIcon(IconUtils::createRecoloredIcon(
        Icon::Database, IconUtils::DefaultIconColor));
    rootItem->appendRow(allDatasetsItem);

    // Add "Domains" parent item
    auto* domainsItem = new QStandardItem(tr("Domains"));
    domainsItem->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
    domainsItem->setIcon(IconUtils::createRecoloredIcon(
        Icon::Folder, IconUtils::DefaultIconColor));
    rootItem->appendRow(domainsItem);
}

void DataLibrarianWindow::setupCentralWorkspace() {
    // Create container for datasets section with title
    auto* datasetsContainer = new QWidget(this);
    auto* datasetsLayout = new QVBoxLayout(datasetsContainer);
    datasetsLayout->setContentsMargins(0, 0, 0, 0);
    datasetsLayout->setSpacing(4);

    auto* datasetsLabel = new QLabel(tr("<b>Datasets</b>"), datasetsContainer);
    datasetsLayout->addWidget(datasetsLabel);

    // Configure dataset table
    datasetProxyModel_->setSourceModel(datasetModel_);
    datasetProxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    datasetTable_->setModel(datasetProxyModel_);
    datasetTable_->setItemDelegate(new DatasetItemDelegate(datasetTable_));
    datasetTable_->setSortingEnabled(true);
    datasetTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    datasetTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    datasetTable_->setAlternatingRowColors(true);
    datasetTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    datasetTable_->horizontalHeader()->setStretchLastSection(true);
    datasetTable_->verticalHeader()->setVisible(false);
    datasetTable_->sortByColumn(ClientDatasetModel::Name, Qt::AscendingOrder);
    datasetTable_->setFrameShape(QFrame::StyledPanel);
    datasetTable_->setFrameShadow(QFrame::Sunken);
    datasetsLayout->addWidget(datasetTable_, 1);

    // Add datasets container to main splitter
    mainSplitter_->addWidget(datasetsContainer);

    // Set splitter sizes: sidebar 300px, dataset table takes the rest
    mainSplitter_->setSizes({300, 1100});
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
    connect(viewDatasetAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onViewDatasetClicked);

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
    connect(methodologyModel_, &ClientMethodologyModel::dataLoaded,
            this, &DataLibrarianWindow::onMethodologiesLoaded);

    // Header context menu for column visibility
    QHeaderView* header = datasetTable_->horizontalHeader();
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &DataLibrarianWindow::showHeaderContextMenu);
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
    case NavigationItemType::Domain:
        if (!itemName.isEmpty()) {
            filterDatasetsByDomain(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    case NavigationItemType::SubjectArea:
        if (!itemName.isEmpty()) {
            filterDatasetsBySubjectArea(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    case NavigationItemType::Catalog:
        if (!itemName.isEmpty()) {
            filterDatasetsByCatalog(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    }
}

void DataLibrarianWindow::onDatasetSelectionChanged() {
    // Enable/disable view action based on selection
    const auto selection = datasetTable_->selectionModel()->selectedRows();
    viewDatasetAction_->setEnabled(!selection.isEmpty());
}

void DataLibrarianWindow::onDatasetDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    const auto sourceIndex = datasetProxyModel_->mapToSource(index);
    const auto* dataset = datasetModel_->getDataset(sourceIndex.row());

    if (dataset) {
        showDatasetDetailDialog(dataset);
    }
}

void DataLibrarianWindow::onViewDatasetClicked() {
    const auto selection = datasetTable_->selectionModel()->selectedRows();
    if (selection.isEmpty()) {
        return;
    }

    const auto sourceIndex = datasetProxyModel_->mapToSource(selection.first());
    const auto* dataset = datasetModel_->getDataset(sourceIndex.row());

    if (dataset) {
        showDatasetDetailDialog(dataset);
    }
}

void DataLibrarianWindow::showDatasetDetailDialog(const dq::domain::dataset* dataset) {
    if (!dataset) {
        return;
    }

    // Create dialog if it doesn't exist
    if (!datasetViewDialog_) {
        datasetViewDialog_ = new DatasetViewDialog(this);
        datasetViewDialog_->setAttribute(Qt::WA_DeleteOnClose, false);
    }

    // Collect methodologies for the dialog
    std::vector<dq::domain::methodology> methodologies;
    for (int i = 0; i < methodologyModel_->rowCount(); ++i) {
        const auto* methodology = methodologyModel_->getMethodology(i);
        if (methodology) {
            methodologies.push_back(*methodology);
        }
    }
    datasetViewDialog_->setMethodologies(methodologies);
    datasetViewDialog_->setDataset(*dataset);

    // Show modeless dialog
    datasetViewDialog_->show();
    datasetViewDialog_->raise();
    datasetViewDialog_->activateWindow();
}

void DataLibrarianWindow::onRefreshClicked() {
    BOOST_LOG_SEV(lg(), info) << "Refresh clicked - reloading all data";

    // Reset and show progress bar
    totalLoads_ = total_model_loads;
    pendingLoads_ = totalLoads_;
    loadingProgressBar_->setRange(0, totalLoads_);
    loadingProgressBar_->setValue(0);
    loadingProgressBar_->setVisible(true);
    statusLabel_->setText(tr("Refreshing domains..."));

    emit statusChanged(tr("Refreshing data..."));
    dataDomainModel_->refresh();
    subjectAreaModel_->refresh();
    catalogModel_->loadData();
    methodologyModel_->refresh();
    datasetModel_->refresh();
}

void DataLibrarianWindow::onDataLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Datasets loaded: " << datasetModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 datasets").arg(datasetModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
        selectFirstDataset();
    }
    emit statusChanged(tr("Loaded %1 datasets").arg(datasetModel_->rowCount()));
}

void DataLibrarianWindow::onLoadError(const QString& error_message,
                                       const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString()
                               << " - " << details.toStdString();
    statusLabel_->setText(tr("Error: %1").arg(error_message));
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void DataLibrarianWindow::onDomainsLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Domains loaded: " << dataDomainModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 domains...").arg(dataDomainModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::onSubjectAreasLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Subject areas loaded: " << subjectAreaModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 subject areas...").arg(subjectAreaModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::onCatalogsLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Catalogs loaded: " << catalogModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 catalogs...").arg(catalogModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::onMethodologiesLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Methodologies loaded: " << methodologyModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 methodologies...").arg(methodologyModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
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
            Icon::Library, IconUtils::DefaultIconColor));
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
                Icon::Folder, IconUtils::DefaultIconColor));

            // Find subject areas for this domain (using domain_name string FK)
            for (int s = 0; s < subjectAreaModel_->rowCount(); ++s) {
                const auto* subjectArea = subjectAreaModel_->getSubjectArea(s);
                if (!subjectArea || subjectArea->domain_name != domain->name) {
                    continue;
                }

                QString saName = QString::fromStdString(subjectArea->name);
                auto* subjectAreaItem = new QStandardItem(saName);
                subjectAreaItem->setData(
                    static_cast<int>(NavigationItemType::SubjectArea), ItemTypeRole);
                subjectAreaItem->setData(saName, ItemIdRole);
                subjectAreaItem->setIcon(IconUtils::createRecoloredIcon(
                    iconForSubjectArea(saName), IconUtils::DefaultIconColor));

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
                Icon::Library, IconUtils::DefaultIconColor));

            catalogsParent->appendRow(catalogItem);
        }
    }

    // Expand all tree items so hierarchy is visible on startup
    navigationTree_->expandAll();
}

void DataLibrarianWindow::filterDatasetsByCatalog(const QString& catalogName) {
    selectedCatalogName_ = catalogName;
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();

    // Filter by catalog column
    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Catalog);
    datasetProxyModel_->setFilterFixedString(catalogName);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by catalog: "
                               << catalogName.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsByDomain(const QString& domainName) {
    selectedDomainName_ = domainName;
    selectedCatalogName_.clear();
    selectedSubjectAreaName_.clear();

    // Filter by domain column
    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Domain);
    datasetProxyModel_->setFilterFixedString(domainName);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by domain: "
                               << domainName.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsBySubjectArea(const QString& subjectAreaName) {
    selectedSubjectAreaName_ = subjectAreaName;
    selectedCatalogName_.clear();
    selectedDomainName_.clear();

    // Filter by subject area column
    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::SubjectArea);
    datasetProxyModel_->setFilterFixedString(subjectAreaName);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by subject area: "
                               << subjectAreaName.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::clearDatasetFilter() {
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    datasetProxyModel_->setFilterRegularExpression("");
    selectFirstDataset();
}

void DataLibrarianWindow::selectFirstDataset() {
    if (datasetProxyModel_->rowCount() > 0) {
        const auto firstIndex = datasetProxyModel_->index(0, 0);
        datasetTable_->selectionModel()->select(
            firstIndex, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
        datasetTable_->setCurrentIndex(firstIndex);
    }
}

void DataLibrarianWindow::setupColumnVisibility() {
    QHeaderView* header = datasetTable_->horizontalHeader();

    // Check for saved settings
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("DataLibrarianWindow");

    if (settings.contains("headerState")) {
        header->restoreState(settings.value("headerState").toByteArray());
        BOOST_LOG_SEV(lg(), debug) << "Restored header state from settings";
    } else {
        applyDefaultColumnVisibility();
    }

    settings.endGroup();
}

void DataLibrarianWindow::applyDefaultColumnVisibility() {
    QHeaderView* header = datasetTable_->horizontalHeader();

    // Hide these columns by default (data is shown in accession card or Tags):
    header->setSectionHidden(ClientDatasetModel::SubjectArea, true);
    header->setSectionHidden(ClientDatasetModel::Domain, true);
    header->setSectionHidden(ClientDatasetModel::Origin, true);
    header->setSectionHidden(ClientDatasetModel::Nature, true);
    header->setSectionHidden(ClientDatasetModel::Treatment, true);
    header->setSectionHidden(ClientDatasetModel::RecordedBy, true);
    header->setSectionHidden(ClientDatasetModel::RecordedAt, true);

    // Reorder columns: Name, Version, SourceSystem, AsOfDate, Tags, Catalog
    header->moveSection(header->visualIndex(ClientDatasetModel::Version), 1);
    header->moveSection(header->visualIndex(ClientDatasetModel::SourceSystem), 2);
    header->moveSection(header->visualIndex(ClientDatasetModel::AsOfDate), 3);
    header->moveSection(header->visualIndex(ClientDatasetModel::Tags), 4);
    header->moveSection(header->visualIndex(ClientDatasetModel::Catalog), 5);

    // Set reasonable column widths
    header->resizeSection(ClientDatasetModel::Name, 240);
    header->resizeSection(ClientDatasetModel::Version, 55);
    header->resizeSection(ClientDatasetModel::SourceSystem, 100);
    header->resizeSection(ClientDatasetModel::AsOfDate, 90);
    header->resizeSection(ClientDatasetModel::Catalog, 120);
    header->resizeSection(ClientDatasetModel::Tags, 180);

    BOOST_LOG_SEV(lg(), debug) << "Applied default column visibility and order";
}

void DataLibrarianWindow::showHeaderContextMenu(const QPoint& pos) {
    QHeaderView* header = datasetTable_->horizontalHeader();
    QMenu menu(this);
    menu.setTitle(tr("Columns"));

    // Add action for each column
    for (int col = 0; col < datasetModel_->columnCount(); ++col) {
        QString columnName = datasetModel_->headerData(col, Qt::Horizontal,
            Qt::DisplayRole).toString();

        QAction* action = menu.addAction(columnName);
        action->setCheckable(true);
        action->setChecked(!header->isSectionHidden(col));

        connect(action, &QAction::toggled, this, [this, header, col](bool visible) {
            header->setSectionHidden(col, !visible);

            // Save settings
            QSettings settings("OreStudio", "OreStudio");
            settings.beginGroup("DataLibrarianWindow");
            settings.setValue("headerState", header->saveState());
            settings.endGroup();

            BOOST_LOG_SEV(lg(), debug) << "Column " << col
                                       << " visibility changed to: " << visible;
        });
    }

    menu.exec(header->mapToGlobal(pos));
}

}
