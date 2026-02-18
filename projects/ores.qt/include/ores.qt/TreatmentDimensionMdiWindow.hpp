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
#ifndef ORES_QT_TREATMENT_DIMENSION_MDI_WINDOW_HPP
#define ORES_QT_TREATMENT_DIMENSION_MDI_WINDOW_HPP

#include <QTableView>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientTreatmentDimensionModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/treatment_dimension.hpp"

namespace ores::qt {

class TreatmentDimensionMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.treatment_dimension_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TreatmentDimensionMdiWindow(ClientManager* clientManager,
                                       const QString& username,
                                       QWidget* parent = nullptr);
    ~TreatmentDimensionMdiWindow() override = default;

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showDimensionDetails(const dq::domain::treatment_dimension& dimension);
    void showDimensionHistory(const QString& code);
    void addNewRequested();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onAddClicked();
    void onEditClicked();
    void onDeleteClicked();
    void onRefreshClicked();
    void onSelectionChanged();
    void onHistoryClicked();

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh treatment dimensions");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QString username_;
    ClientTreatmentDimensionModel* model_;
    QSortFilterProxyModel* proxyModel_;
    QTableView* tableView_;
    QToolBar* toolbar_;

    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* refreshAction_;
    QAction* historyAction_;
};

}

#endif
