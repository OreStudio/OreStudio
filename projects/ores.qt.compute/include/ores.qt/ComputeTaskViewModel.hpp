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
#ifndef ORES_QT_COMPUTE_TASK_VIEW_MODEL_HPP
#define ORES_QT_COMPUTE_TASK_VIEW_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HostDisplayNameCache.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.compute.api/domain/result.hpp"

namespace ores::qt {

/**
 * @brief A joined view row: one result with its workunit and batch context.
 */
struct compute_task {
    compute::domain::result   result;
    compute::domain::workunit workunit;
    compute::domain::batch    batch;
    int batch_ordinal = 0; ///< 1-based position in the loaded batch list
    int task_ordinal  = 0; ///< 1-based position within its batch
};

/**
 * @brief Read-only table model that joins results, workunits, and batches.
 *
 * Issues three async NATS requests (batches, workunits, results) in a single
 * background task, joins them by UUID, and presents one row per result.
 * The Host column is resolved lazily via HostDisplayNameCache.
 */
class ComputeTaskViewModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.compute_task_view_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Label,
        State,
        Outcome,
        Host,
        Duration,
        Batch,
        Received,
        ColumnCount
    };

    explicit ComputeTaskViewModel(ClientManager* clientManager,
        QObject* parent = nullptr);
    ~ComputeTaskViewModel() override = default;

    static QString format_state(int server_state);
    static QString format_outcome(int outcome);

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
        int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Fetch and join all batches, workunits, and results.
     */
    void refresh();

    /**
     * @brief Returns the task at the given row, or nullptr if out of range.
     */
    const compute_task* get_task(int row) const;

    /**
     * @brief Sets the host display-name cache (not owned).
     */
    void set_host_name_cache(HostDisplayNameCache* cache);

private slots:
    void onTasksLoaded();

private:
    struct FetchResult {
        bool success = false;
        std::vector<compute_task> tasks;
        QString error_message;
        QString error_details;
    };

    void fetch_tasks();

    ClientManager* clientManager_;
    HostDisplayNameCache* host_name_cache_{nullptr};
    std::vector<compute_task> tasks_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};
};

} // namespace ores::qt

#endif
