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
#ifndef ORES_QT_DATA_LIBRARIAN_WINDOW_HPP
#define ORES_QT_DATA_LIBRARIAN_WINDOW_HPP

#include <QWidget>
#include <QSplitter>
#include <QTreeView>
#include <QTableView>
#include <QToolBar>
#include <QStatusBar>
#include <QLabel>
#include <QProgressBar>
#include <QSortFilterProxyModel>
#include <QStandardItemModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientDatasetModel.hpp"
#include "ores.qt/ClientDataDomainModel.hpp"
#include "ores.qt/ClientSubjectAreaModel.hpp"
#include "ores.qt/ClientCatalogModel.hpp"
#include "ores.qt/ClientDatasetDependencyModel.hpp"
#include "ores.qt/ClientMethodologyModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/methodology.hpp"

namespace ores::qt {

/**
 * @brief The Data Librarian window for browsing and managing datasets.
 *
 * This composite window provides:
 * - Navigation sidebar ("The Stacks"): Tree view of Domain > Subject Area > Catalog
 * - Central workspace ("The Registry"): Dataset table with filtering
 * - Detail panel ("Dataset Accession Card"): Inline view of selected dataset
 * - Lineage visualizer: Simple flow diagram of data provenance
 *
 * The window also provides toolbar buttons to open related entity windows
 * (Dimensions, Coding Schemes, Methodologies).
 */
class DataLibrarianWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.data_librarian_window";
    static constexpr int total_model_loads = 6;

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit DataLibrarianWindow(ClientManager* clientManager,
                                  const QString& username,
                                  QWidget* parent = nullptr);
    ~DataLibrarianWindow() override = default;

    QSize sizeHint() const override { return QSize(1600, 900); }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

    /**
     * @brief Emitted when datasets are successfully published.
     *
     * Connect to this signal to trigger cache refreshes (e.g., ImageCache).
     */
    void datasetsPublished();

    // Signals to open related windows
    void openOriginDimensionsRequested();
    void openNatureDimensionsRequested();
    void openTreatmentDimensionsRequested();
    void openCodingSchemesRequested();
    void openMethodologiesRequested();
    void openDataDomainsRequested();
    void openSubjectAreasRequested();
    void openCatalogsRequested();

private slots:
    void onNavigationSelectionChanged(const QModelIndex& current,
                                       const QModelIndex& previous);
    void onDatasetSelectionChanged();
    void onDatasetDoubleClicked(const QModelIndex& index);
    void onRefreshClicked();
    void onViewDatasetClicked();
    void onPublishClicked();
    void onPublicationHistoryClicked();
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});

    // Navigation data loaded
    void onDomainsLoaded();
    void onSubjectAreasLoaded();
    void onCatalogsLoaded();
    void onDatasetDependenciesLoaded();
    void onMethodologiesLoaded();

    // Column visibility context menu
    void showHeaderContextMenu(const QPoint& pos);

    // Dataset context menu
    void showDatasetContextMenu(const QPoint& pos);

private:
    void setupUi();
    void setupNavigationSidebar();
    void setupCentralWorkspace();
    void setupToolbar();
    void setupConnections();
    void buildNavigationTree();
    void showDatasetDetailDialog(const dq::domain::dataset* dataset);
    void filterDatasetsByCatalog(const QString& catalogName);
    void filterDatasetsByDomain(const QString& domainName);
    void filterDatasetsBySubjectArea(const QString& subjectAreaName);
    void clearDatasetFilter();
    void selectFirstDataset();
    void setupColumnVisibility();
    void applyDefaultColumnVisibility();

    ClientManager* clientManager_;
    QString username_;

    // Main layout
    QSplitter* mainSplitter_;
    QSplitter* centralSplitter_;

    // Navigation sidebar ("The Stacks")
    QTreeView* navigationTree_;
    QStandardItemModel* navigationModel_;

    // Toolbar
    QToolBar* toolbar_;
    QAction* refreshAction_;
    QAction* viewDatasetAction_;
    QAction* publishAction_;
    QAction* publicationHistoryAction_;
    QAction* originDimensionsAction_;
    QAction* natureDimensionsAction_;
    QAction* treatmentDimensionsAction_;
    QAction* codingSchemesAction_;
    QAction* methodologiesAction_;

    // Central workspace - Dataset table
    QTableView* datasetTable_;
    ClientDatasetModel* datasetModel_;
    QSortFilterProxyModel* datasetProxyModel_;

    // Data models for navigation
    ClientDataDomainModel* dataDomainModel_;
    ClientSubjectAreaModel* subjectAreaModel_;
    ClientCatalogModel* catalogModel_;
    ClientDatasetDependencyModel* datasetDependencyModel_;
    ClientMethodologyModel* methodologyModel_;

    // Status bar with loading indicator
    QStatusBar* statusBar_;
    QProgressBar* loadingProgressBar_;
    QLabel* statusLabel_;
    int pendingLoads_{0};
    int totalLoads_{0};

    // Track selected filter
    QString selectedCatalogName_;
    QString selectedDomainName_;
    QString selectedSubjectAreaName_;
};

}

#endif
