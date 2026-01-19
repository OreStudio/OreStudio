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

    // Load data
    statusLabel_->setText(tr("Loading data..."));
    dataDomainModel_->refresh();
    subjectAreaModel_->refresh();
    catalogModel_->loadData();
    methodologyModel_->refresh();
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
    datasetTable_->setItemDelegate(new DatasetItemDelegate(datasetTable_));
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
    catalogLabel_->setToolTip(tr("The catalog this dataset belongs to"));
    classificationLayout->addRow(tr("Catalog:"), catalogLabel_);

    classificationLayout->addRow(new QLabel(tr("")));

    auto* originRowLabel = new QLabel(tr("Origin:"), detailPanel_);
    originRowLabel->setToolTip(tr(
        "Origin indicates where the data came from:\n"
        "- Source: Primary data directly from originating system\n"
        "- Derived: Data computed or aggregated from other datasets"));
    originLabel_ = new QLabel(tr("-"), detailPanel_);
    originLabel_->setToolTip(originRowLabel->toolTip());
    classificationLayout->addRow(originRowLabel, originLabel_);

    auto* natureRowLabel = new QLabel(tr("Nature:"), detailPanel_);
    natureRowLabel->setToolTip(tr(
        "Nature describes the type of values in the data:\n"
        "- Actual: Real observed values\n"
        "- Estimated: Calculated or projected values\n"
        "- Simulated: Model-generated values"));
    natureLabel_ = new QLabel(tr("-"), detailPanel_);
    natureLabel_->setToolTip(natureRowLabel->toolTip());
    classificationLayout->addRow(natureRowLabel, natureLabel_);

    auto* treatmentRowLabel = new QLabel(tr("Treatment:"), detailPanel_);
    treatmentRowLabel->setToolTip(tr(
        "Treatment indicates how the data has been processed:\n"
        "- Raw: Unprocessed original data\n"
        "- Cleaned: Data with corrections applied\n"
        "- Enriched: Data augmented with additional information"));
    treatmentLabel_ = new QLabel(tr("-"), detailPanel_);
    treatmentLabel_->setToolTip(treatmentRowLabel->toolTip());
    classificationLayout->addRow(treatmentRowLabel, treatmentLabel_);

    contentLayout->addWidget(classificationGroup);

    // Center: Lineage & Methodology
    auto* lineageGroup = new QGroupBox(tr("The Trace (Lineage)"), detailPanel_);
    auto* lineageLayout = new QVBoxLayout(lineageGroup);

    methodologyLabel_ = new QLabel(tr("Methodology: -"), detailPanel_);
    methodologyLabel_->setToolTip(tr("The methodology used to produce this dataset"));
    lineageLayout->addWidget(methodologyLabel_);

    descriptionLabel_ = new QLabel(tr(""), detailPanel_);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setStyleSheet("color: #888; font-style: italic;");
    lineageLayout->addWidget(descriptionLabel_);

    // Lineage visualization - allow it to grow
    lineageView_ = new QGraphicsView(detailPanel_);
    lineageView_->setMinimumHeight(80);
    lineageView_->setScene(new QGraphicsScene(lineageView_));
    lineageView_->setRenderHint(QPainter::Antialiasing);
    lineageLayout->addWidget(lineageView_, 1);

    contentLayout->addWidget(lineageGroup, 1);

    detailLayout->addLayout(contentLayout, 1);

    // Add detail panel to central splitter
    centralSplitter_->addWidget(detailPanel_);

    // Set central splitter sizes: table 65%, detail 35%
    centralSplitter_->setSizes({500, 250});

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
    // Reset and show progress bar
    totalLoads_ = 5;
    pendingLoads_ = totalLoads_;
    loadingProgressBar_->setRange(0, totalLoads_);
    loadingProgressBar_->setValue(0);
    loadingProgressBar_->setVisible(true);
    statusLabel_->setText(tr("Refreshing data..."));

    emit statusChanged(tr("Refreshing data..."));
    dataDomainModel_->refresh();
    subjectAreaModel_->refresh();
    catalogModel_->loadData();
    methodologyModel_->refresh();
    datasetModel_->refresh();
}

void DataLibrarianWindow::onDataLoaded() {
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Loaded %1 datasets").arg(datasetModel_->rowCount()));
    }
    emit statusChanged(tr("Loaded %1 datasets").arg(datasetModel_->rowCount()));
}

void DataLibrarianWindow::onLoadError(const QString& error_message,
                                       const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    statusLabel_->setText(tr("Error: %1").arg(error_message));
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void DataLibrarianWindow::onDomainsLoaded() {
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
    }
    BOOST_LOG_SEV(lg(), debug) << "Domains loaded: "
                               << dataDomainModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::onSubjectAreasLoaded() {
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
    }
    BOOST_LOG_SEV(lg(), debug) << "Subject areas loaded: "
                               << subjectAreaModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::onCatalogsLoaded() {
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
    }
    BOOST_LOG_SEV(lg(), debug) << "Catalogs loaded: "
                               << catalogModel_->rowCount();
    buildNavigationTree();
}

void DataLibrarianWindow::onMethodologiesLoaded() {
    --pendingLoads_;
    loadingProgressBar_->setValue(totalLoads_ - pendingLoads_);
    if (pendingLoads_ <= 0) {
        loadingProgressBar_->setVisible(false);
        statusLabel_->setText(tr("Ready"));
    }
    BOOST_LOG_SEV(lg(), debug) << "Methodologies loaded: "
                               << methodologyModel_->rowCount();
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

    const qreal boxWidth = 90;
    const qreal boxHeight = 36;
    const qreal spacing = 30;
    const qreal padding = 6;

    QFont smallFont;
    smallFont.setPointSize(7);
    QFontMetrics fm(smallFont);

    auto elidedText = [&fm, boxWidth, padding](const QString& text) {
        return fm.elidedText(text, Qt::ElideRight, static_cast<int>(boxWidth - 2 * padding));
    };

    auto centerTextInBox = [&fm, boxHeight, padding](QGraphicsTextItem* textItem, qreal boxX) {
        qreal textHeight = fm.height();
        qreal textY = (boxHeight - textHeight) / 2;
        textItem->setPos(boxX + padding, textY);
    };

    // Source box (Origin dimension)
    scene->addRect(0, 0, boxWidth, boxHeight, QPen(lineColor), QBrush(boxColor));
    auto* sourceText = scene->addText(elidedText(QString::fromStdString(dataset->origin_code)));
    sourceText->setFont(smallFont);
    sourceText->setDefaultTextColor(textColor);
    centerTextInBox(sourceText, 0);

    // Methodology box
    qreal methodX = boxWidth + spacing;
    scene->addRect(methodX, 0, boxWidth, boxHeight, QPen(lineColor), QBrush(boxColor));
    auto* methodText = scene->addText(elidedText(findMethodologyName(dataset->methodology_id)));
    methodText->setFont(smallFont);
    methodText->setDefaultTextColor(textColor);
    centerTextInBox(methodText, methodX);

    // Dataset box
    qreal datasetX = 2 * (boxWidth + spacing);
    scene->addRect(datasetX, 0, boxWidth, boxHeight, QPen(lineColor), QBrush(boxColor));
    auto* datasetText = scene->addText(elidedText(QString::fromStdString(dataset->name)));
    datasetText->setFont(smallFont);
    datasetText->setDefaultTextColor(textColor);
    centerTextInBox(datasetText, datasetX);

    // Arrows
    QPen arrowPen(lineColor, 2);

    // Arrow 1: Source -> Methodology
    scene->addLine(boxWidth, boxHeight / 2, methodX, boxHeight / 2, arrowPen);

    // Arrow 2: Methodology -> Dataset
    scene->addLine(methodX + boxWidth, boxHeight / 2, datasetX, boxHeight / 2, arrowPen);

    lineageView_->fitInView(scene->sceneRect(), Qt::KeepAspectRatio);
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

    // Reorder columns: Name, Version, SourceSystem, AsOfDate, Catalog, Tags
    header->moveSection(header->visualIndex(ClientDatasetModel::Version), 1);
    header->moveSection(header->visualIndex(ClientDatasetModel::SourceSystem), 2);
    header->moveSection(header->visualIndex(ClientDatasetModel::AsOfDate), 3);
    header->moveSection(header->visualIndex(ClientDatasetModel::Catalog), 4);
    header->moveSection(header->visualIndex(ClientDatasetModel::Tags), 5);

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
    if (!id) {
        return tr("None");
    }

    // Look up from methodology model
    for (int i = 0; i < methodologyModel_->rowCount(); ++i) {
        const auto* methodology = methodologyModel_->getMethodology(i);
        if (methodology && methodology->id == *id) {
            return QString::fromStdString(methodology->name);
        }
    }

    // Fallback to truncated UUID if not found
    return QString::fromStdString(boost::uuids::to_string(*id)).left(8) + "...";
}

QString DataLibrarianWindow::findCatalogName(
    const boost::uuids::uuid& /*id*/) const {
    // Not used - dataset stores catalog_name directly
    return tr("N/A");
}

}
