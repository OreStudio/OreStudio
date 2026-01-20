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
#ifndef ORES_QT_DATASET_VIEW_DIALOG_HPP
#define ORES_QT_DATASET_VIEW_DIALOG_HPP

#include <map>
#include <vector>
#include <QDialog>
#include <QTabWidget>
#include <QTreeWidget>
#include <QTextBrowser>
#include <QGraphicsView>
#include <QGraphicsScene>
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/methodology.hpp"
#include "ores.dq/domain/dataset_dependency.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {
class ClientManager;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing dataset details with tabbed interface.
 *
 * Displays dataset information organized into tabs:
 * - Overview: general info, classification, data quality dimensions, audit info
 * - Provenance: methodology, source, dates, license, lineage info
 * - Methodology: methodology details and processing steps
 * - Lineage: visual diagram
 */
class DatasetViewDialog : public QDialog {
    Q_OBJECT

    // QSS-styleable properties for lineage diagram
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
    Q_PROPERTY(QColor lineageHeaderCatalog MEMBER lineageHeaderCatalog_)
    Q_PROPERTY(qreal lineageNodeWidth MEMBER lineageNodeWidth_)
    Q_PROPERTY(qreal lineageHeaderHeight MEMBER lineageHeaderHeight_)
    Q_PROPERTY(qreal lineageRowHeight MEMBER lineageRowHeight_)
    Q_PROPERTY(qreal lineageNodeSpacing MEMBER lineageNodeSpacing_)
    Q_PROPERTY(qreal lineageCornerRadius MEMBER lineageCornerRadius_)
    Q_PROPERTY(qreal lineageSocketRadius MEMBER lineageSocketRadius_)
    Q_PROPERTY(qreal lineagePadding MEMBER lineagePadding_)

private:
    inline static std::string_view logger_name = "ores.qt.dataset_view_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit DatasetViewDialog(ClientManager* clientManager,
                               QWidget* parent = nullptr);
    ~DatasetViewDialog() override;

    void setDataset(const dq::domain::dataset& dataset);
    void setMethodologies(const std::vector<dq::domain::methodology>& methodologies);
    void setDatasetDependencies(
        const std::vector<dq::domain::dataset_dependency>& dependencies);
    void setDatasetNames(const std::map<std::string, std::string>& codeToName);

private:
    void setupUi();
    QWidget* createOverviewTab();
    QWidget* createProvenanceTab();
    QWidget* createMethodologyTab();
    QWidget* createLineageTab();

    void updateOverviewTab();
    void updateProvenanceTab();
    void updateMethodologyTab();
    void updateLineageView();

    // Helper to add property to tree widget
    void addProperty(QTreeWidget* tree, const QString& name, const QString& value,
        const QString& tooltip = {});
    void addSectionHeader(QTreeWidget* tree, const QString& title);

    QString findMethodologyName(const std::optional<boost::uuids::uuid>& methodologyId) const;
    const dq::domain::methodology* findMethodology(
        const std::optional<boost::uuids::uuid>& methodologyId) const;

    // Lineage diagram helper methods
    qreal createLineageNode(QGraphicsScene* scene, qreal x, qreal y,
        const QString& headerText, const QStringList& labels,
        const QStringList& values, const QColor& headerColor,
        bool hasInputSocket, bool hasOutputSocket) const;
    void createLineageNodeBody(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeHeight, const QString& tooltip) const;
    void createLineageNodeHeader(QGraphicsScene* scene, qreal x, qreal y,
        const QString& headerText, const QColor& headerColor,
        const QString& tooltip) const;
    void createLineageNodeProperties(QGraphicsScene* scene, qreal x, qreal y,
        const QStringList& labels, const QStringList& values) const;
    void createLineageNodeSockets(QGraphicsScene* scene, qreal x, qreal y,
        qreal nodeHeight, bool hasInputSocket, bool hasOutputSocket) const;
    void drawLineageConnection(QGraphicsScene* scene,
        qreal x1, qreal y1, qreal x2, qreal y2) const;

    // Tab widget
    QTabWidget* tabWidget_;

    // Overview tab (General + Classification + Data Quality + Audit)
    QTreeWidget* overviewTree_;

    // Provenance tab
    QTreeWidget* provenanceTree_;

    // Methodology tab
    QTreeWidget* methodologyTree_;
    QTextBrowser* stepsText_;

    // Lineage tab
    QGraphicsView* lineageView_;

    // Data
    ClientManager* clientManager_;
    dq::domain::dataset dataset_;
    std::vector<dq::domain::methodology> methodologies_;
    std::vector<dq::domain::dataset_dependency> datasetDependencies_;
    std::map<std::string, std::string> datasetNames_;  // code -> name lookup

    // Lineage styling properties (QSS-configurable)
    QColor lineageBackground_{0x2D, 0x2D, 0x30};
    QColor lineageNodeBody_{0x3F, 0x3F, 0x46};
    QColor lineageNodeBorder_{0x52, 0x52, 0x5B};
    QColor lineageText_{0xFF, 0xFF, 0xFF};
    QColor lineageLabel_{0xA1, 0xA1, 0xAA};
    QColor lineageValue_{0xE4, 0xE4, 0xE7};
    QColor lineageConnection_{0x71, 0x71, 0x7A};
    QColor lineageSocket_{0x3B, 0x82, 0xF6};
    QColor lineageHeaderOrigin_{0x3B, 0x82, 0xF6};
    QColor lineageHeaderMethod_{0x8B, 0x5C, 0xF6};
    QColor lineageHeaderDataset_{0x22, 0xC5, 0x5E};
    QColor lineageHeaderCatalog_{0xF9, 0x73, 0x16};  // Orange for catalog
    qreal lineageNodeWidth_{200};  // Wider to accommodate long dataset names
    qreal lineageHeaderHeight_{18};
    qreal lineageRowHeight_{14};
    qreal lineageNodeSpacing_{60};
    qreal lineageCornerRadius_{4};
    qreal lineageSocketRadius_{4};
    qreal lineagePadding_{4};
};

}

#endif
