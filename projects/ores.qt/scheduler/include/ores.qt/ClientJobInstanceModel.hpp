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
#ifndef ORES_QT_CLIENT_JOB_INSTANCE_MODEL_HPP
#define ORES_QT_CLIENT_JOB_INSTANCE_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"

namespace ores::qt {

/**
 * @brief Table model for the global job-instance execution history.
 *
 * Fetches data via scheduler.v1.job-instances.list and exposes it as a
 * read-only QAbstractTableModel for display in JobInstanceMdiWindow.
 */
class ClientJobInstanceModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_job_instance_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        JobName,
        Status,
        TriggeredAt,
        StartedAt,
        Duration,
        ActionType,
        ErrorMessage,
        ColumnCount
    };

    explicit ClientJobInstanceModel(ClientManager* clientManager,
                                    QObject* parent = nullptr);
    ~ClientJobInstanceModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh();

    const scheduler::messaging::job_instance_summary* getInstance(int row) const;

private slots:
    void onInstancesLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<scheduler::messaging::job_instance_summary> instances;
        QString error_message;
        QString error_details;
    };

    void fetch_instances();

    ClientManager* clientManager_;
    std::vector<scheduler::messaging::job_instance_summary> instances_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};
};

}

#endif
