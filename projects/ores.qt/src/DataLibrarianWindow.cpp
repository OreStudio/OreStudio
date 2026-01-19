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
#include <QMenu>
#include <QSettings>
#include <QPushButton>
#include <QTextBrowser>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/DatasetItemDelegate.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

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

QString iconPathForSubjectArea(const QString& subjectAreaName) {
    if (subjectAreaName == "Countries") {
        return ":/icons/ic_fluent_globe_20_regular.svg";
    } else if (subjectAreaName == "Currencies") {
        return ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg";
    } else if (subjectAreaName == "Cryptocurrencies") {
        return ":/icons/ic_fluent_currency_dollar_euro_20_regular.svg";
    }
    return ":/icons/ic_fluent_table_20_regular.svg";
}

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
    setupDetailPanel();
    setupLineagePanel();
    setupConnections();
    setupColumnVisibility();

    // Configure loading progress bar
    totalLoads_ = 5;
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

    // Related windows - dimensions
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
    // Create left panel container with Data Browser and Detail Panel stacked vertically
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

    // Dataset Accession Card section (detail panel)
    auto* cardLabel = new QLabel(tr("<b>Dataset Accession Card</b>"), leftPanel);
    leftLayout->addWidget(cardLabel);
    leftLayout->addWidget(detailPanel_, 1);

    // Set minimum/maximum width for left panel (wide enough for GUIDs)
    leftPanel->setMinimumWidth(350);
    leftPanel->setMaximumWidth(500);

    // Add to main splitter
    mainSplitter_->addWidget(leftPanel);

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

    // Add datasets container to central splitter
    centralSplitter_->addWidget(datasetsContainer);

    // Add central splitter to main splitter
    mainSplitter_->addWidget(centralSplitter_);

    // Set splitter sizes: sidebar 350px, central takes the rest
    mainSplitter_->setSizes({350, 1050});
}

void DataLibrarianWindow::setupDetailPanel() {
    auto* detailLayout = new QVBoxLayout(detailPanel_);
    detailLayout->setContentsMargins(0, 0, 0, 0);
    detailLayout->setSpacing(4);

    // Properties tree - styled to match dataset list view
    propertiesTree_ = new QTreeWidget(detailPanel_);
    propertiesTree_->setColumnCount(2);
    propertiesTree_->setHeaderHidden(true);  // Hide headers to match list view
    propertiesTree_->setRootIsDecorated(false);
    propertiesTree_->setAlternatingRowColors(true);
    propertiesTree_->setSelectionMode(QAbstractItemView::NoSelection);
    propertiesTree_->setFocusPolicy(Qt::NoFocus);
    propertiesTree_->setIndentation(0);
    propertiesTree_->setFrameShape(QFrame::StyledPanel);
    propertiesTree_->setFrameShadow(QFrame::Sunken);
    propertiesTree_->header()->setStretchLastSection(true);
    propertiesTree_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    // Match row height to dataset table
    propertiesTree_->setStyleSheet("QTreeWidget::item { padding: 2px 4px; }");
    detailLayout->addWidget(propertiesTree_, 1);

    // Methodology section header
    auto* methodologyHeader = new QLabel(tr("<b>Methodology</b>"), detailPanel_);
    detailLayout->addWidget(methodologyHeader);

    // Methodology content
    auto* methodologyWidget = new QWidget(detailPanel_);
    auto* methodologyLayout = new QVBoxLayout(methodologyWidget);
    methodologyLayout->setContentsMargins(4, 4, 4, 4);
    methodologyLayout->setSpacing(2);

    methodologyLabel_ = new QLabel(tr("-"), methodologyWidget);
    methodologyLabel_->setToolTip(tr("The methodology used to produce this dataset"));
    methodologyLayout->addWidget(methodologyLabel_);

    sourceUrlLabel_ = new QLabel(tr(""), methodologyWidget);
    sourceUrlLabel_->setOpenExternalLinks(true);
    sourceUrlLabel_->setTextFormat(Qt::RichText);
    sourceUrlLabel_->setToolTip(tr("Source URL for the data"));
    sourceUrlLabel_->setWordWrap(true);
    methodologyLayout->addWidget(sourceUrlLabel_);

    descriptionLabel_ = new QLabel(tr(""), methodologyWidget);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setStyleSheet("color: #888; font-style: italic;");
    methodologyLayout->addWidget(descriptionLabel_);

    // Implementation details (collapsible)
    implementationDetailsToggle_ = new QPushButton(tr("▶ Processing Steps"), methodologyWidget);
    implementationDetailsToggle_->setFlat(true);
    implementationDetailsToggle_->setStyleSheet(
        "QPushButton { text-align: left; padding: 2px 0; color: #666; font-size: 11px; }"
        "QPushButton:hover { color: #888; }");
    implementationDetailsToggle_->setCursor(Qt::PointingHandCursor);
    methodologyLayout->addWidget(implementationDetailsToggle_);

    implementationDetailsText_ = new QTextBrowser(methodologyWidget);
    implementationDetailsText_->setOpenExternalLinks(true);
    implementationDetailsText_->setFrameShape(QFrame::NoFrame);
    implementationDetailsText_->setStyleSheet(
        "QTextBrowser { background-color: #f5f5f5; font-family: monospace; font-size: 10px; padding: 4px; }");
    implementationDetailsText_->setMaximumHeight(150);
    implementationDetailsText_->setVisible(false);
    methodologyLayout->addWidget(implementationDetailsText_);

    // Connect toggle button
    connect(implementationDetailsToggle_, &QPushButton::clicked, this, [this]() {
        bool visible = !implementationDetailsText_->isVisible();
        implementationDetailsText_->setVisible(visible);
        implementationDetailsToggle_->setText(visible ? tr("▼ Processing Steps") : tr("▶ Processing Steps"));
    });

    detailLayout->addWidget(methodologyWidget);

    // Initially hide detail panel until a dataset is selected
    detailPanel_->setVisible(false);
}

void DataLibrarianWindow::setupLineagePanel() {
    lineagePanel_ = new QWidget(this);
    auto* lineageLayout = new QVBoxLayout(lineagePanel_);
    lineageLayout->setContentsMargins(0, 0, 0, 0);
    lineageLayout->setSpacing(4);

    // Lineage title
    auto* lineageLabel = new QLabel(tr("<b>Lineage</b>"), lineagePanel_);
    lineageLayout->addWidget(lineageLabel);

    // Lineage visualization
    lineageView_ = new QGraphicsView(lineagePanel_);
    lineageView_->setMinimumHeight(100);
    lineageView_->setScene(new QGraphicsScene(lineageView_));
    lineageView_->setRenderHint(QPainter::Antialiasing);
    lineageView_->setFrameShape(QFrame::StyledPanel);
    lineageView_->setFrameShadow(QFrame::Sunken);
    lineageLayout->addWidget(lineageView_, 1);

    // Add to central splitter (below dataset table)
    centralSplitter_->addWidget(lineagePanel_);

    // Set central splitter sizes: table 50%, lineage 50%
    centralSplitter_->setSizes({400, 400});

    // Initially hide lineage panel until a dataset is selected
    lineagePanel_->setVisible(false);
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
    const auto selected = datasetTable_->selectionModel()->selectedRows();

    if (selected.isEmpty()) {
        detailPanel_->setVisible(false);
        lineagePanel_->setVisible(false);
        return;
    }

    const auto sourceIndex = datasetProxyModel_->mapToSource(selected.first());
    const auto* dataset = datasetModel_->getDataset(sourceIndex.row());

    updateDetailPanel(dataset);
    detailPanel_->setVisible(true);
    lineagePanel_->setVisible(true);
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
    BOOST_LOG_SEV(lg(), info) << "Refresh clicked - reloading all data";

    // Reset and show progress bar
    totalLoads_ = 5;
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

                QString saName = QString::fromStdString(subjectArea->name);
                auto* subjectAreaItem = new QStandardItem(saName);
                subjectAreaItem->setData(
                    static_cast<int>(NavigationItemType::SubjectArea), ItemTypeRole);
                subjectAreaItem->setData(saName, ItemIdRole);
                subjectAreaItem->setIcon(IconUtils::createRecoloredIcon(
                    iconPathForSubjectArea(saName), color_constants::icon_color));

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

    // Expand all tree items so hierarchy is visible on startup
    navigationTree_->expandAll();
}

void DataLibrarianWindow::updateDetailPanel(const dq::domain::dataset* dataset) {
    propertiesTree_->clear();

    // Helper to add a property row with tooltip
    auto addProperty = [this](const QString& name, const QString& value,
                              const QString& tooltip = {}) {
        auto* item = new QTreeWidgetItem(propertiesTree_);
        item->setText(0, name);
        item->setText(1, value);
        if (!tooltip.isEmpty()) {
            item->setToolTip(0, tooltip);
            item->setToolTip(1, tooltip);
        }
        return item;
    };

    if (!dataset) {
        addProperty(tr("Name"), tr("-"));
        addProperty(tr("ID"), tr("-"));
        addProperty(tr("Catalog"), tr("-"));
        addProperty(tr("Subject Area"), tr("-"));
        addProperty(tr("Origin"), tr("-"));
        addProperty(tr("Nature"), tr("-"));
        addProperty(tr("Treatment"), tr("-"));
        addProperty(tr("Recorded By"), tr("-"));
        addProperty(tr("Recorded At"), tr("-"));
        addProperty(tr("Change Commentary"), tr("-"));
        methodologyLabel_->setText(tr("-"));
        sourceUrlLabel_->setText(tr(""));
        descriptionLabel_->setText(tr(""));
        implementationDetailsToggle_->setVisible(false);
        implementationDetailsText_->setVisible(false);
        implementationDetailsText_->clear();
        return;
    }

    // Basic info
    addProperty(tr("Name"), QString::fromStdString(dataset->name),
        tr("The name of this dataset"));
    addProperty(tr("ID"), QString::fromStdString(boost::uuids::to_string(dataset->id)),
        tr("Unique identifier for this dataset"));
    addProperty(tr("Catalog"),
        dataset->catalog_name ? QString::fromStdString(*dataset->catalog_name) : tr("-"),
        tr("The catalog this dataset belongs to"));
    addProperty(tr("Subject Area"), QString::fromStdString(dataset->subject_area_name),
        tr("The subject area this dataset belongs to"));

    // Classification dimensions
    addProperty(tr("Origin"), QString::fromStdString(dataset->origin_code),
        tr("Origin indicates where the data came from:\n"
           "- Primary: Data directly from originating system\n"
           "- Derived: Data computed or aggregated from other datasets"));
    addProperty(tr("Nature"), QString::fromStdString(dataset->nature_code),
        tr("Nature describes the type of values in the data:\n"
           "- Actual: Real observed values\n"
           "- Estimated: Calculated or projected values\n"
           "- Simulated: Model-generated values"));
    addProperty(tr("Treatment"), QString::fromStdString(dataset->treatment_code),
        tr("Treatment indicates how the data has been processed:\n"
           "- Raw: Unprocessed original data\n"
           "- Cleaned: Data with corrections applied\n"
           "- Enriched: Data augmented with additional information"));

    // Audit info
    addProperty(tr("Recorded By"), QString::fromStdString(dataset->recorded_by),
        tr("The user who recorded this dataset"));
    addProperty(tr("Recorded At"), relative_time_helper::format(dataset->recorded_at),
        tr("When this dataset was recorded"));
    addProperty(tr("Change Commentary"),
        dataset->change_commentary.empty() ? tr("-") : QString::fromStdString(dataset->change_commentary),
        tr("Commentary explaining the last change to this dataset"));

    // Methodology and source URL
    const auto* methodology = findMethodology(dataset->methodology_id);
    if (methodology) {
        methodologyLabel_->setText(QString::fromStdString(methodology->name));
        if (methodology->logic_reference && !methodology->logic_reference->empty()) {
            QString url = QString::fromStdString(*methodology->logic_reference);
            sourceUrlLabel_->setText(
                QString("<a href=\"%1\">%1</a>").arg(url.toHtmlEscaped()));
        } else {
            sourceUrlLabel_->setText(tr(""));
        }
        // Implementation details (processing steps)
        if (methodology->implementation_details && !methodology->implementation_details->empty()) {
            implementationDetailsToggle_->setVisible(true);
            implementationDetailsText_->setPlainText(
                QString::fromStdString(*methodology->implementation_details));
        } else {
            implementationDetailsToggle_->setVisible(false);
            implementationDetailsText_->setVisible(false);
            implementationDetailsText_->clear();
        }
    } else {
        methodologyLabel_->setText(dataset->methodology_id ? tr("Unknown") : tr("None"));
        sourceUrlLabel_->setText(tr(""));
        implementationDetailsToggle_->setVisible(false);
        implementationDetailsText_->setVisible(false);
        implementationDetailsText_->clear();
    }

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

    // Node-based lineage flow inspired by Blender's node editor
    // Colors
    const QColor bgColor(45, 45, 48);           // Dark background
    const QColor nodeBodyColor(63, 63, 70);     // Node body
    const QColor nodeBorderColor(80, 80, 85);   // Node border
    const QColor textColor(220, 220, 220);      // Light text
    const QColor labelColor(140, 140, 145);     // Dim text for labels
    const QColor valueColor(180, 180, 185);     // Slightly brighter for values
    const QColor connectionColor(180, 180, 180);// Connection lines
    const QColor socketColor(200, 200, 200);    // Socket circles

    // Header colors for different node types
    const QColor originHeaderColor(74, 144, 226);    // Blue for origin/input
    const QColor methodHeaderColor(130, 94, 186);    // Purple for methodology
    const QColor datasetHeaderColor(80, 200, 120);   // Green for output/dataset

    // Dimensions
    const qreal nodeWidth = 95;
    const qreal headerHeight = 14;
    const qreal rowHeight = 11;
    const qreal nodeSpacing = 40;
    const qreal cornerRadius = 3;
    const qreal socketRadius = 3;
    const qreal padding = 4;

    // Fonts
    QFont headerFont;
    headerFont.setPointSize(6);
    headerFont.setBold(true);
    QFont labelFont;
    labelFont.setPointSize(5);
    QFont valueFont;
    valueFont.setPointSize(5);
    valueFont.setBold(true);
    QFontMetrics headerFm(headerFont);
    QFontMetrics labelFm(labelFont);
    QFontMetrics valueFm(valueFont);

    // Helper to create a node with header and multiple property rows
    auto createNode = [&](qreal x, qreal y, const QString& headerText,
                          const QStringList& labels, const QStringList& values,
                          const QColor& headerColor,
                          bool hasInputSocket, bool hasOutputSocket) -> qreal {
        int numRows = std::min(labels.size(), values.size());
        qreal bodyHeight = numRows * rowHeight + padding * 2;
        qreal nodeHeight = headerHeight + bodyHeight;

        // Build tooltip with all properties
        QString nodeTooltip = QString("<b>%1</b>").arg(headerText);
        for (int i = 0; i < numRows; ++i) {
            nodeTooltip += QString("<br>%1: %2").arg(labels[i], values[i]);
        }

        // Node body (rounded rect) - acts as tooltip area
        QPainterPath bodyPath;
        bodyPath.addRoundedRect(x, y, nodeWidth, nodeHeight, cornerRadius, cornerRadius);
        auto* bodyItem = scene->addPath(bodyPath, QPen(nodeBorderColor), QBrush(nodeBodyColor));
        bodyItem->setToolTip(nodeTooltip);

        // Header background (top rounded, bottom square)
        QPainterPath headerPath;
        headerPath.moveTo(x + cornerRadius, y);
        headerPath.arcTo(x, y, cornerRadius * 2, cornerRadius * 2, 90, 90);
        headerPath.lineTo(x, y + headerHeight);
        headerPath.lineTo(x + nodeWidth, y + headerHeight);
        headerPath.lineTo(x + nodeWidth, y + cornerRadius);
        headerPath.arcTo(x + nodeWidth - cornerRadius * 2, y, cornerRadius * 2, cornerRadius * 2, 0, 90);
        headerPath.closeSubpath();
        auto* headerBg = scene->addPath(headerPath, QPen(Qt::NoPen), QBrush(headerColor));
        headerBg->setToolTip(nodeTooltip);

        // Header text (centered)
        QString elidedHeader = headerFm.elidedText(headerText, Qt::ElideRight,
            static_cast<int>(nodeWidth - 2 * padding));
        auto* headerItem = scene->addText(elidedHeader);
        headerItem->setFont(headerFont);
        headerItem->setDefaultTextColor(textColor);
        headerItem->setToolTip(nodeTooltip);
        qreal headerTextWidth = headerFm.horizontalAdvance(elidedHeader);
        headerItem->setPos(x + (nodeWidth - headerTextWidth) / 2,
                          y + (headerHeight - headerFm.height()) / 2 - 1);

        // Property rows
        qreal rowY = y + headerHeight + padding;
        int maxLabelWidth = static_cast<int>(nodeWidth * 0.35);
        int maxValueWidth = static_cast<int>(nodeWidth - maxLabelWidth - padding * 3);

        for (int i = 0; i < numRows; ++i) {
            // Label (left aligned)
            QString elidedLabel = labelFm.elidedText(labels[i] + ":", Qt::ElideRight, maxLabelWidth);
            auto* labelItem = scene->addText(elidedLabel);
            labelItem->setFont(labelFont);
            labelItem->setDefaultTextColor(labelColor);
            labelItem->setPos(x + padding, rowY + (rowHeight - labelFm.height()) / 2);
            labelItem->setToolTip(QString("%1: %2").arg(labels[i], values[i]));

            // Value (right of label) - with tooltip showing full value
            QString elidedValue = valueFm.elidedText(values[i], Qt::ElideRight, maxValueWidth);
            auto* valueItem = scene->addText(elidedValue);
            valueItem->setFont(valueFont);
            valueItem->setDefaultTextColor(valueColor);
            valueItem->setPos(x + padding + maxLabelWidth + 2, rowY + (rowHeight - valueFm.height()) / 2);
            valueItem->setToolTip(QString("%1: %2").arg(labels[i], values[i]));

            rowY += rowHeight;
        }

        // Input socket (left side, vertically centered)
        if (hasInputSocket) {
            qreal socketY = y + nodeHeight / 2;
            scene->addEllipse(x - socketRadius, socketY - socketRadius,
                socketRadius * 2, socketRadius * 2,
                QPen(nodeBorderColor), QBrush(socketColor));
        }

        // Output socket (right side, vertically centered)
        if (hasOutputSocket) {
            qreal socketY = y + nodeHeight / 2;
            scene->addEllipse(x + nodeWidth - socketRadius, socketY - socketRadius,
                socketRadius * 2, socketRadius * 2,
                QPen(nodeBorderColor), QBrush(socketColor));
        }

        return nodeHeight;
    };

    // Helper to draw curved connection between nodes
    auto drawConnection = [&](qreal x1, qreal y1, qreal x2, qreal y2) {
        QPainterPath path;
        path.moveTo(x1, y1);
        qreal ctrlOffset = (x2 - x1) * 0.4;
        path.cubicTo(x1 + ctrlOffset, y1,
                     x2 - ctrlOffset, y2,
                     x2, y2);
        scene->addPath(path, QPen(connectionColor, 1.2));
    };

    // Calculate positions
    qreal node1X = 0;
    qreal node2X = nodeWidth + nodeSpacing;
    qreal node3X = 2 * (nodeWidth + nodeSpacing);
    qreal nodeY = 0;

    // Create nodes with multiple properties
    qreal node1Height = createNode(node1X, nodeY, tr("Primary"),
        {tr("Origin"), tr("Nature"), tr("Treatment")},
        {QString::fromStdString(dataset->origin_code),
         QString::fromStdString(dataset->nature_code),
         QString::fromStdString(dataset->treatment_code)},
        originHeaderColor, false, true);

    qreal node2Height = createNode(node2X, nodeY, tr("Process"),
        {tr("Method"), tr("System")},
        {findMethodologyName(dataset->methodology_id),
         QString::fromStdString(dataset->source_system_id)},
        methodHeaderColor, true, true);

    qreal node3Height = createNode(node3X, nodeY, tr("Output"),
        {tr("Name"), tr("Version"), tr("As Of")},
        {QString::fromStdString(dataset->name),
         QString::number(dataset->version),
         relative_time_helper::format(dataset->as_of_date)},
        datasetHeaderColor, true, false);

    // Draw connections (from output socket to input socket)
    // Use the tallest node height to center connections
    qreal maxHeight = std::max({node1Height, node2Height, node3Height});
    qreal connY1 = nodeY + node1Height / 2;
    qreal connY2 = nodeY + node2Height / 2;
    qreal connY3 = nodeY + node3Height / 2;
    Q_UNUSED(maxHeight);

    drawConnection(node1X + nodeWidth + socketRadius, connY1,
                   node2X - socketRadius, connY2);
    drawConnection(node2X + nodeWidth + socketRadius, connY2,
                   node3X - socketRadius, connY3);

    // Set scene background
    scene->setBackgroundBrush(QBrush(bgColor));

    // Fit view with some margin
    QRectF sceneRect = scene->itemsBoundingRect();
    sceneRect.adjust(-8, -8, 8, 8);
    lineageView_->fitInView(sceneRect, Qt::KeepAspectRatio);
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
}

void DataLibrarianWindow::clearDatasetFilter() {
    selectedCatalogName_.clear();
    selectedDomainName_.clear();
    selectedSubjectAreaName_.clear();
    datasetProxyModel_->setFilterRegularExpression("");
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
    const auto* methodology = findMethodology(id);
    if (methodology) {
        return QString::fromStdString(methodology->name);
    }
    if (!id) {
        return tr("None");
    }
    // Fallback to truncated UUID if not found
    return QString::fromStdString(boost::uuids::to_string(*id)).left(8) + "...";
}

const dq::domain::methodology* DataLibrarianWindow::findMethodology(
    const std::optional<boost::uuids::uuid>& id) const {
    if (!id) {
        return nullptr;
    }

    // Look up from methodology model
    for (int i = 0; i < methodologyModel_->rowCount(); ++i) {
        const auto* methodology = methodologyModel_->getMethodology(i);
        if (methodology && methodology->id == *id) {
            return methodology;
        }
    }
    return nullptr;
}

QString DataLibrarianWindow::findCatalogName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores catalog_name directly
    return tr("N/A");
}

}
