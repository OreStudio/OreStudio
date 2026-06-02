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
#ifndef ORES_QT_COMPUTE_TRANSFER_MODEL_HPP
#define ORES_QT_COMPUTE_TRANSFER_MODEL_HPP

#include <vector>
#include <QString>
#include <QAbstractTableModel>

namespace ores::qt {

/**
 * @brief A single in-flight or completed file transfer.
 */
struct transfer_item {
    QString id;        ///< Unique token (e.g. result_id + direction)
    QString direction; ///< "↑" (upload) or "↓" (download)
    QString filename;
    int     progress = 0;   ///< 0–100
    QString speed;          ///< e.g. "1.2 MB/s"; empty when idle
    QString status;         ///< "Transferring", "Complete", "Failed"
};

/**
 * @brief Pure Qt model tracking upload/download progress.
 *
 * No NATS; driven entirely by signals from upload/download helpers.
 * Completed and failed transfers remain visible until explicitly cleared.
 */
class ComputeTransferModel final : public QAbstractTableModel {
    Q_OBJECT

public:
    enum Column {
        Direction,
        Filename,
        Progress,
        Speed,
        Status,
        ColumnCount
    };

    explicit ComputeTransferModel(QObject* parent = nullptr);

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
        int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Adds a new transfer entry and returns its row index.
     */
    int add_transfer(const QString& id,
        const QString& direction,
        const QString& filename);

    /**
     * @brief Updates progress (0–100) and speed string for an existing entry.
     */
    void update_progress(const QString& id, int progress,
        const QString& speed = {});

    /**
     * @brief Marks a transfer as complete (progress = 100).
     */
    void complete_transfer(const QString& id);

    /**
     * @brief Marks a transfer as failed.
     */
    void fail_transfer(const QString& id, const QString& reason = {});

    /**
     * @brief Removes all completed and failed entries.
     */
    void clear_finished();

private:
    int find_row(const QString& id) const;

    std::vector<transfer_item> transfers_;
};

} // namespace ores::qt

#endif
