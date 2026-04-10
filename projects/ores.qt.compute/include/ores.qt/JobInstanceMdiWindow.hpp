/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_JOB_INSTANCE_MDI_WINDOW_HPP
#define ORES_QT_JOB_INSTANCE_MDI_WINDOW_HPP

#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientJobInstanceModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing the global job-instance execution history.
 *
 * Read-only list of all recent job instance runs across all definitions.
 * Double-clicking a row opens a JobInstanceDetailDialog.
 */
class JobInstanceMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.job_instance_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit JobInstanceMdiWindow(
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~JobInstanceMdiWindow() override = default;

public slots:
    void doReload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showInstanceDetails(const scheduler::messaging::job_instance_summary& instance);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh job instances");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onDoubleClicked(const QModelIndex& index);

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientJobInstanceModel* model_;
    QSortFilterProxyModel* proxyModel_;

    QAction* reloadAction_;
};

}

#endif
