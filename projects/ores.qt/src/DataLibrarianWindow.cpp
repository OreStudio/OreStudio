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

#include <map>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QMenu>
#include <QSettings>
#include <QFutureWatcher>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/DatasetItemDelegate.hpp"
#include "ores.qt/DatasetViewDialog.hpp"
#include "ores.qt/PublishDatasetsDialog.hpp"
#include "ores.qt/PublicationHistoryDialog.hpp"
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"

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
    Catalog,
    Bundle,
    OriginDimension,
    NatureDimension,
    TreatmentDimension
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
      datasetDependencyModel_(new ClientDatasetDependencyModel(clientManager, this)),
      methodologyModel_(new ClientMethodologyModel(clientManager, this)),
      bundleModel_(new ClientDatasetBundleModel(clientManager, this)),
      originDimensionModel_(new ClientOriginDimensionModel(clientManager, this)),
      natureDimensionModel_(new ClientNatureDimensionModel(clientManager, this)),
      treatmentDimensionModel_(new ClientTreatmentDimensionModel(clientManager, this)),
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

    statusLabel_->setText(tr("Loading dataset dependencies..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting dataset dependencies...";
    datasetDependencyModel_->loadData();

    statusLabel_->setText(tr("Loading methodologies..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting methodologies...";
    methodologyModel_->refresh();

    statusLabel_->setText(tr("Loading datasets..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting datasets...";
    datasetModel_->refresh();

    statusLabel_->setText(tr("Loading bundles..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting bundles...";
    bundleModel_->refresh();

    statusLabel_->setText(tr("Loading dimensions..."));
    BOOST_LOG_SEV(lg(), debug) << "Requesting origin dimensions...";
    originDimensionModel_->refresh();

    BOOST_LOG_SEV(lg(), debug) << "Requesting nature dimensions...";
    natureDimensionModel_->refresh();

    BOOST_LOG_SEV(lg(), debug) << "Requesting treatment dimensions...";
    treatmentDimensionModel_->refresh();
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

    publishAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Publish, IconUtils::DefaultIconColor),
        tr("Publish"));
    publishAction_->setToolTip(tr("Publish selected datasets to production tables"));
    publishAction_->setEnabled(false);

    publicationHistoryAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    publicationHistoryAction_->setToolTip(tr("View publication history"));

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
    datasetTable_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    datasetTable_->setContextMenuPolicy(Qt::CustomContextMenu);
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

    // Dataset context menu
    connect(datasetTable_, &QTableView::customContextMenuRequested,
            this, &DataLibrarianWindow::showDatasetContextMenu);

    // Toolbar actions
    connect(refreshAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onRefreshClicked);
    connect(viewDatasetAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onViewDatasetClicked);
    connect(publishAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onPublishClicked);
    connect(publicationHistoryAction_, &QAction::triggered,
            this, &DataLibrarianWindow::onPublicationHistoryClicked);

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
    connect(datasetDependencyModel_, &ClientDatasetDependencyModel::loadFinished,
            this, &DataLibrarianWindow::onDatasetDependenciesLoaded);
    connect(methodologyModel_, &ClientMethodologyModel::dataLoaded,
            this, &DataLibrarianWindow::onMethodologiesLoaded);

    connect(bundleModel_, &ClientDatasetBundleModel::dataLoaded,
            this, &DataLibrarianWindow::onBundlesLoaded);
    connect(originDimensionModel_, &ClientOriginDimensionModel::dataLoaded,
            this, &DataLibrarianWindow::onOriginDimensionsLoaded);
    connect(natureDimensionModel_, &ClientNatureDimensionModel::dataLoaded,
            this, &DataLibrarianWindow::onNatureDimensionsLoaded);
    connect(treatmentDimensionModel_, &ClientTreatmentDimensionModel::dataLoaded,
            this, &DataLibrarianWindow::onTreatmentDimensionsLoaded);

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
    case NavigationItemType::Bundle:
        if (!itemName.isEmpty()) {
            filterDatasetsByBundle(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    case NavigationItemType::OriginDimension:
        if (!itemName.isEmpty()) {
            filterDatasetsByOrigin(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    case NavigationItemType::NatureDimension:
        if (!itemName.isEmpty()) {
            filterDatasetsByNature(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    case NavigationItemType::TreatmentDimension:
        if (!itemName.isEmpty()) {
            filterDatasetsByTreatment(itemName);
        } else {
            clearDatasetFilter();
        }
        break;
    }
}

void DataLibrarianWindow::onDatasetSelectionChanged() {
    // Enable/disable view/publish actions based on selection
    const auto selection = datasetTable_->selectionModel()->selectedRows();
    viewDatasetAction_->setEnabled(!selection.isEmpty());
    publishAction_->setEnabled(!selection.isEmpty());
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

void DataLibrarianWindow::onPublishClicked() {
    const auto selection = datasetTable_->selectionModel()->selectedRows();
    if (selection.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "No datasets selected for publish";
        return;
    }

    // Collect selected datasets
    std::vector<dq::domain::dataset> selectedDatasets;
    selectedDatasets.reserve(selection.size());

    for (const auto& proxyIndex : selection) {
        const auto sourceIndex = datasetProxyModel_->mapToSource(proxyIndex);
        const auto* dataset = datasetModel_->getDataset(sourceIndex.row());
        if (dataset) {
            selectedDatasets.push_back(*dataset);
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Opening publish dialog for "
        << selectedDatasets.size() << " datasets";

    // Open publish dialog
    auto* dialog = new PublishDatasetsDialog(clientManager_, username_, this);
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->setDatasets(selectedDatasets);

    // Forward published signal (for cache refresh)
    connect(dialog, &PublishDatasetsDialog::datasetsPublished,
            this, &DataLibrarianWindow::datasetsPublished);

    dialog->exec();
}

void DataLibrarianWindow::onPublicationHistoryClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Opening publication history dialog";

    auto* dialog = new PublicationHistoryDialog(clientManager_, this);
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->refresh();
    dialog->show();
}

void DataLibrarianWindow::showDatasetDetailDialog(const dq::domain::dataset* dataset) {
    if (!dataset) {
        return;
    }

    // Create a new dialog for each dataset (allows multiple windows)
    auto* dialog = new DatasetViewDialog(clientManager_, this);
    dialog->setAttribute(Qt::WA_DeleteOnClose);  // Auto-delete when closed

    // Collect methodologies for the dialog
    std::vector<dq::domain::methodology> methodologies;
    for (int i = 0; i < methodologyModel_->rowCount(); ++i) {
        const auto* methodology = methodologyModel_->getMethodology(i);
        if (methodology) {
            methodologies.push_back(*methodology);
        }
    }
    dialog->setMethodologies(methodologies);
    dialog->setDatasetDependencies(datasetDependencyModel_->dependencies());

    // Build code-to-name lookup for dependency display
    std::map<std::string, std::string> datasetNames;
    for (int i = 0; i < datasetModel_->rowCount(); ++i) {
        const auto* ds = datasetModel_->getDataset(i);
        if (ds) {
            datasetNames[ds->code] = ds->name;
        }
    }
    dialog->setDatasetNames(datasetNames);

    dialog->setDataset(*dataset);

    // Show modeless dialog
    dialog->show();
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
    datasetDependencyModel_->loadData();
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

void DataLibrarianWindow::onDatasetDependenciesLoaded() {
    const auto& deps = datasetDependencyModel_->dependencies();
    BOOST_LOG_SEV(lg(), info) << "Catalog dependencies loaded: " << deps.size()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 dataset dependencies...").arg(deps.size()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
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

void DataLibrarianWindow::onBundlesLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Bundles loaded: " << bundleModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 bundles...").arg(bundleModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();

    // Fetch bundle members to populate the cache for filtering
    fetchBundleMembers();
}

void DataLibrarianWindow::onOriginDimensionsLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Origin dimensions loaded: " << originDimensionModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 origin dimensions...").arg(originDimensionModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::onNatureDimensionsLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Nature dimensions loaded: " << natureDimensionModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 nature dimensions...").arg(natureDimensionModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::onTreatmentDimensionsLoaded() {
    BOOST_LOG_SEV(lg(), info) << "Treatment dimensions loaded: " << treatmentDimensionModel_->rowCount()
                              << " (pending: " << pendingLoads_ - 1 << ")";
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    statusLabel_->setText(tr("Loaded %1 treatment dimensions...").arg(treatmentDimensionModel_->rowCount()));
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
        BOOST_LOG_SEV(lg(), info) << "All data loading complete";
    }
    buildNavigationTree();
}

void DataLibrarianWindow::fetchBundleMembers() {
    BOOST_LOG_SEV(lg(), debug) << "Fetching bundle members...";

    auto* self = this;
    auto* watcher = new QFutureWatcher<std::vector<dq::domain::dataset_bundle_member>>(this);

    connect(watcher, &QFutureWatcher<std::vector<dq::domain::dataset_bundle_member>>::finished,
            this, [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        // Populate the bundle member cache
        self->bundleMemberCache_.clear();
        for (const auto& member : result) {
            QString bundleCode = QString::fromStdString(member.bundle_code);
            QString datasetCode = QString::fromStdString(member.dataset_code);
            self->bundleMemberCache_[bundleCode].append(datasetCode);
        }

        BOOST_LOG_SEV(self->lg(), info) << "Bundle members loaded: " << result.size()
                                         << " members across "
                                         << self->bundleMemberCache_.size() << " bundles";
    });

    auto task = [self]() -> std::vector<dq::domain::dataset_bundle_member> {
        dq::messaging::get_dataset_bundle_members_request request;
        auto result = self->clientManager_->process_request(std::move(request));
        if (result) {
            return std::move(result->members);
        }
        return {};
    };

    watcher->setFuture(QtConcurrent::run(task));
}

void DataLibrarianWindow::onBundleMembersLoaded() {
    // This slot is no longer needed as we handle it inline in fetchBundleMembers
    // Keeping it for compatibility with the header declaration
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
    QStandardItem* bundlesParent = nullptr;
    QStandardItem* dimensionsParent = nullptr;

    for (int i = 0; i < rootItem->rowCount(); ++i) {
        auto* item = rootItem->child(i);
        if (item->text() == tr("Domains")) {
            domainsParent = item;
        } else if (item->text() == tr("Catalogs")) {
            catalogsParent = item;
        } else if (item->text() == tr("Bundles")) {
            bundlesParent = item;
        } else if (item->text() == tr("Dimensions")) {
            dimensionsParent = item;
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

    // Create Bundles parent if not exists
    if (!bundlesParent) {
        bundlesParent = new QStandardItem(tr("Bundles"));
        bundlesParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        bundlesParent->setIcon(IconUtils::createRecoloredIcon(
            Icon::Folder, IconUtils::DefaultIconColor));
        rootItem->appendRow(bundlesParent);
    }

    // Create Dimensions parent if not exists
    if (!dimensionsParent) {
        dimensionsParent = new QStandardItem(tr("Dimensions"));
        dimensionsParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        dimensionsParent->setIcon(IconUtils::createRecoloredIcon(
            Icon::Table, IconUtils::DefaultIconColor));
        rootItem->appendRow(dimensionsParent);
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

    // Build Bundles list
    if (bundlesParent) {
        bundlesParent->removeRows(0, bundlesParent->rowCount());

        for (int b = 0; b < bundleModel_->rowCount(); ++b) {
            const auto* bundle = bundleModel_->getBundle(b);
            if (!bundle) continue;

            auto* bundleItem = new QStandardItem(
                QString::fromStdString(bundle->name));
            bundleItem->setData(
                static_cast<int>(NavigationItemType::Bundle), ItemTypeRole);
            bundleItem->setData(
                QString::fromStdString(bundle->code), ItemIdRole);
            bundleItem->setIcon(IconUtils::createRecoloredIcon(
                Icon::Folder, IconUtils::DefaultIconColor));

            bundlesParent->appendRow(bundleItem);
        }
    }

    // Build Dimensions section with three sub-categories
    if (dimensionsParent) {
        dimensionsParent->removeRows(0, dimensionsParent->rowCount());

        // Origin Dimension
        auto* originParent = new QStandardItem(tr("Origin"));
        originParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        originParent->setIcon(IconUtils::createRecoloredIcon(
            Icon::Globe, IconUtils::DefaultIconColor));

        for (int i = 0; i < originDimensionModel_->rowCount(); ++i) {
            const auto* dim = originDimensionModel_->getDimension(i);
            if (!dim) continue;

            auto* dimItem = new QStandardItem(QString::fromStdString(dim->name));
            dimItem->setData(static_cast<int>(NavigationItemType::OriginDimension), ItemTypeRole);
            dimItem->setData(QString::fromStdString(dim->code), ItemIdRole);
            dimItem->setIcon(IconUtils::createRecoloredIcon(
                Icon::Record, IconUtils::DefaultIconColor));
            originParent->appendRow(dimItem);
        }
        dimensionsParent->appendRow(originParent);

        // Nature Dimension
        auto* natureParent = new QStandardItem(tr("Nature"));
        natureParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        natureParent->setIcon(IconUtils::createRecoloredIcon(
            Icon::Star, IconUtils::DefaultIconColor));

        for (int i = 0; i < natureDimensionModel_->rowCount(); ++i) {
            const auto* dim = natureDimensionModel_->getDimension(i);
            if (!dim) continue;

            auto* dimItem = new QStandardItem(QString::fromStdString(dim->name));
            dimItem->setData(static_cast<int>(NavigationItemType::NatureDimension), ItemTypeRole);
            dimItem->setData(QString::fromStdString(dim->code), ItemIdRole);
            dimItem->setIcon(IconUtils::createRecoloredIcon(
                Icon::Record, IconUtils::DefaultIconColor));
            natureParent->appendRow(dimItem);
        }
        dimensionsParent->appendRow(natureParent);

        // Treatment Dimension
        auto* treatmentParent = new QStandardItem(tr("Treatment"));
        treatmentParent->setData(static_cast<int>(NavigationItemType::Root), ItemTypeRole);
        treatmentParent->setIcon(IconUtils::createRecoloredIcon(
            Icon::Settings, IconUtils::DefaultIconColor));

        for (int i = 0; i < treatmentDimensionModel_->rowCount(); ++i) {
            const auto* dim = treatmentDimensionModel_->getDimension(i);
            if (!dim) continue;

            auto* dimItem = new QStandardItem(QString::fromStdString(dim->name));
            dimItem->setData(static_cast<int>(NavigationItemType::TreatmentDimension), ItemTypeRole);
            dimItem->setData(QString::fromStdString(dim->code), ItemIdRole);
            dimItem->setIcon(IconUtils::createRecoloredIcon(
                Icon::Record, IconUtils::DefaultIconColor));
            treatmentParent->appendRow(dimItem);
        }
        dimensionsParent->appendRow(treatmentParent);
    }

    // Expand all tree items so hierarchy is visible on startup
    navigationTree_->expandAll();
}

void DataLibrarianWindow::filterDatasetsByCatalog(const QString& catalogName) {
    selectedCatalogName_ = catalogName;
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();

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
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();

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
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();

    // Filter by subject area column
    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::SubjectArea);
    datasetProxyModel_->setFilterFixedString(subjectAreaName);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by subject area: "
                               << subjectAreaName.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsByBundle(const QString& bundleCode) {
    selectedBundleCode_ = bundleCode;
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();

    // Get datasets for this bundle from the cache
    auto it = bundleMemberCache_.find(bundleCode);
    if (it != bundleMemberCache_.end() && !it->second.isEmpty()) {
        // Build a regex pattern that matches any of the dataset codes
        QStringList escapedCodes;
        for (const auto& code : it->second) {
            escapedCodes.append(QRegularExpression::escape(code));
        }
        QString pattern = "^(" + escapedCodes.join("|") + ")$";
        datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Code);
        datasetProxyModel_->setFilterRegularExpression(
            QRegularExpression(pattern, QRegularExpression::CaseInsensitiveOption));
    } else {
        // No datasets in bundle - show nothing
        datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Code);
        datasetProxyModel_->setFilterFixedString("__NO_MATCH__");
    }

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by bundle: "
                               << bundleCode.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsByOrigin(const QString& originCode) {
    selectedOriginCode_ = originCode;
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedBundleCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();

    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Origin);
    datasetProxyModel_->setFilterFixedString(originCode);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by origin: "
                               << originCode.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsByNature(const QString& natureCode) {
    selectedNatureCode_ = natureCode;
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedTreatmentCode_.clear();

    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Nature);
    datasetProxyModel_->setFilterFixedString(natureCode);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by nature: "
                               << natureCode.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::filterDatasetsByTreatment(const QString& treatmentCode) {
    selectedTreatmentCode_ = treatmentCode;
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();

    datasetProxyModel_->setFilterKeyColumn(ClientDatasetModel::Treatment);
    datasetProxyModel_->setFilterFixedString(treatmentCode);

    BOOST_LOG_SEV(lg(), debug) << "Filter datasets by treatment: "
                               << treatmentCode.toStdString();
    selectFirstDataset();
}

void DataLibrarianWindow::clearDatasetFilter() {
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    selectedBundleCode_.clear();
    selectedOriginCode_.clear();
    selectedNatureCode_.clear();
    selectedTreatmentCode_.clear();
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

void DataLibrarianWindow::showDatasetContextMenu(const QPoint& pos) {
    const auto selection = datasetTable_->selectionModel()->selectedRows();
    if (selection.isEmpty()) {
        return;
    }

    QMenu menu(this);

    // View action (only for single selection)
    QAction* viewAction = menu.addAction(
        IconUtils::createRecoloredIcon(Icon::Info, IconUtils::DefaultIconColor),
        tr("View Details"));
    viewAction->setEnabled(selection.size() == 1);

    menu.addSeparator();

    // Publish action
    QAction* publishAction = menu.addAction(
        IconUtils::createRecoloredIcon(Icon::Publish, IconUtils::DefaultIconColor),
        selection.size() == 1
            ? tr("Publish Dataset")
            : tr("Publish %1 Datasets").arg(selection.size()));

    // Execute menu and handle selection
    QAction* selectedAction = menu.exec(datasetTable_->viewport()->mapToGlobal(pos));

    if (selectedAction == viewAction) {
        onViewDatasetClicked();
    } else if (selectedAction == publishAction) {
        onPublishClicked();
    }
}

}
