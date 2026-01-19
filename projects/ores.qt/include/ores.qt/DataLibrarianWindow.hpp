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
#include <QTreeWidget>
#include <QTableView>
#include <QToolBar>
#include <QStatusBar>
#include <QLabel>
#include <QLineEdit>
#include <QProgressBar>
#include <QStackedWidget>
#include <QSortFilterProxyModel>
#include <QStandardItemModel>
#include <QGraphicsView>
#include <QTextBrowser>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientDatasetModel.hpp"
#include "ores.qt/ClientDataDomainModel.hpp"
#include "ores.qt/ClientSubjectAreaModel.hpp"
#include "ores.qt/ClientCatalogModel.hpp"
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

    // Lineage diagram colors (styleable via QSS qproperty-)
    Q_PROPERTY(QColor lineageBackground MEMBER lineageBackground_)
    Q_PROPERTY(QColor lineageNodeBody MEMBER lineageNodeBody_)
    Q_PROPERTY(QColor lineageNodeBorder MEMBER lineageNodeBorder_)
    Q_PROPERTY(QColor lineageText MEMBER lineageText_)
    Q_PROPERTY(QColor lineageLabel MEMBER lineageLabel_)
    Q_PROPERTY(QColor lineageValue MEMBER lineageValue_)
    Q_PROPERTY(QColor lineageConnection MEMBER lineageConnection_)
    Q_PROPERTY(QColor lineageSocket MEMBER lineageSocket_)
    Q_PROPERTY(QColor lineageHeaderOrigin MEMBER lineageHeaderOrigin_)
    Q_PROPERTY(QColor lineageHeaderMethod MEMBER lineageHeaderMethod_)
    Q_PROPERTY(QColor lineageHeaderDataset MEMBER lineageHeaderDataset_)

    // Lineage diagram dimensions (styleable via QSS qproperty-)
    Q_PROPERTY(qreal lineageNodeWidth MEMBER lineageNodeWidth_)
    Q_PROPERTY(qreal lineageHeaderHeight MEMBER lineageHeaderHeight_)
    Q_PROPERTY(qreal lineageRowHeight MEMBER lineageRowHeight_)
    Q_PROPERTY(qreal lineageNodeSpacing MEMBER lineageNodeSpacing_)
    Q_PROPERTY(qreal lineageCornerRadius MEMBER lineageCornerRadius_)
    Q_PROPERTY(qreal lineageSocketRadius MEMBER lineageSocketRadius_)
    Q_PROPERTY(qreal lineagePadding MEMBER lineagePadding_)

private:
    inline static std::string_view logger_name = "ores.qt.data_librarian_window";
    static constexpr int total_model_loads = 5;

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
    void showDatasetDetails(const dq::domain::dataset& dataset);
    void showDatasetHistory(const boost::uuids::uuid& id);

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
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});

    // Navigation data loaded
    void onDomainsLoaded();
    void onSubjectAreasLoaded();
    void onCatalogsLoaded();
    void onMethodologiesLoaded();

    // Column visibility context menu
    void showHeaderContextMenu(const QPoint& pos);

private:
    void setupUi();
    void setupNavigationSidebar();
    void setupCentralWorkspace();
    void setupDetailPanel();
    void setupLineagePanel();
    void setupMethodologyPanel();
    void setupToolbar();
    void setupConnections();
    void buildNavigationTree();
    void updateDetailPanel(const dq::domain::dataset* dataset);
    void updateMethodologyPanel(const dq::domain::dataset* dataset);
    void updateLineageView(const dq::domain::dataset* dataset);
    void filterDatasetsByCatalog(const QString& catalogName);
    void filterDatasetsByDomain(const QString& domainName);
    void filterDatasetsBySubjectArea(const QString& subjectAreaName);
    void clearDatasetFilter();
    void selectFirstDataset();
    void setupColumnVisibility();
    void applyDefaultColumnVisibility();

    // Helper to find dimension/methodology names
    QString findOriginDimensionName(const boost::uuids::uuid& id) const;
    QString findNatureDimensionName(const boost::uuids::uuid& id) const;
    QString findTreatmentDimensionName(const boost::uuids::uuid& id) const;
    QString findMethodologyName(const std::optional<boost::uuids::uuid>& id) const;
    const dq::domain::methodology* findMethodology(const std::optional<boost::uuids::uuid>& id) const;
    QString findCatalogName(const boost::uuids::uuid& id) const;

    // Lineage diagram helper methods
    qreal createLineageNode(QGraphicsScene* scene, qreal x, qreal y,
        const QString& headerText, const QStringList& labels,
        const QStringList& values, const QColor& headerColor,
        bool hasInputSocket, bool hasOutputSocket) const;
    void createLineageNodeBody(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeWidth, qreal nodeHeight, const QString& tooltip) const;
    void createLineageNodeHeader(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeWidth, const QString& headerText, const QColor& headerColor,
        const QString& tooltip) const;
    void createLineageNodeProperties(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeWidth, const QStringList& labels, const QStringList& values) const;
    void createLineageNodeSockets(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeWidth, qreal nodeHeight, bool hasInput, bool hasOutput) const;
    void drawLineageConnection(QGraphicsScene* scene, qreal x1, qreal y1,
        qreal x2, qreal y2) const;

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
    QAction* originDimensionsAction_;
    QAction* natureDimensionsAction_;
    QAction* treatmentDimensionsAction_;
    QAction* codingSchemesAction_;
    QAction* methodologiesAction_;

    // Central workspace - Dataset table
    QTableView* datasetTable_;
    ClientDatasetModel* datasetModel_;
    QSortFilterProxyModel* datasetProxyModel_;

    // Detail panel ("Dataset Accession Card")
    QWidget* detailPanel_;
    QTreeWidget* propertiesTree_;  // Unified property list

    // Lineage panel (below dataset table)
    QWidget* lineagePanel_;
    QGraphicsView* lineageView_;

    // Methodology panel (right side)
    QWidget* methodologyPanel_;
    QTreeWidget* methodologyPropertiesTree_;
    QTextBrowser* implementationDetailsText_;

    // Data models for navigation
    ClientDataDomainModel* dataDomainModel_;
    ClientSubjectAreaModel* subjectAreaModel_;
    ClientCatalogModel* catalogModel_;
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

    // Lineage diagram styling (defaults, can be overridden via QSS)
    QColor lineageBackground_{45, 45, 48};
    QColor lineageNodeBody_{63, 63, 70};
    QColor lineageNodeBorder_{80, 80, 85};
    QColor lineageText_{220, 220, 220};
    QColor lineageLabel_{140, 140, 145};
    QColor lineageValue_{180, 180, 185};
    QColor lineageConnection_{180, 180, 180};
    QColor lineageSocket_{200, 200, 200};
    QColor lineageHeaderOrigin_{74, 144, 226};
    QColor lineageHeaderMethod_{130, 94, 186};
    QColor lineageHeaderDataset_{80, 200, 120};
    qreal lineageNodeWidth_{95};
    qreal lineageHeaderHeight_{14};
    qreal lineageRowHeight_{11};
    qreal lineageNodeSpacing_{40};
    qreal lineageCornerRadius_{3};
    qreal lineageSocketRadius_{3};
    qreal lineagePadding_{4};
};

}

#endif
