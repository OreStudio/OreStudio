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
#include "ores.qt/ComputeTransferModel.hpp"

#include <algorithm>

namespace ores::qt {

ComputeTransferModel::ComputeTransferModel(QObject* parent)
    : QAbstractTableModel(parent) {}

int ComputeTransferModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(transfers_.size());
}

int ComputeTransferModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ComputeTransferModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid()) return {};
    const auto row = static_cast<std::size_t>(index.row());
    if (row >= transfers_.size()) return {};
    const auto& t = transfers_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Direction: return t.direction;
        case Filename:  return t.filename;
        case Progress:  return t.progress;
        case Speed:     return t.speed;
        case Status:    return t.status;
        default:        return {};
        }
    }

    // The Progress column value is also exposed as UserRole so the delegate
    // can read it without parsing the display string.
    if (role == Qt::UserRole && index.column() == Progress)
        return t.progress;

    return {};
}

QVariant ComputeTransferModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole) return {};
    switch (section) {
    case Direction: return tr("Dir");
    case Filename:  return tr("File");
    case Progress:  return tr("Progress");
    case Speed:     return tr("Speed");
    case Status:    return tr("Status");
    default:        return {};
    }
}

int ComputeTransferModel::add_transfer(
    const QString& id, const QString& direction, const QString& filename) {
    const int row = static_cast<int>(transfers_.size());
    beginInsertRows({}, row, row);
    transfers_.push_back({
        .id        = id,
        .direction = direction,
        .filename  = filename,
        .progress  = 0,
        .speed     = {},
        .status    = tr("Transferring")
    });
    endInsertRows();
    return row;
}

void ComputeTransferModel::update_progress(
    const QString& id, int progress, const QString& speed) {
    const int row = find_row(id);
    if (row < 0) return;
    auto& t = transfers_[static_cast<std::size_t>(row)];
    t.progress = std::clamp(progress, 0, 100);
    t.speed    = speed;
    emit dataChanged(index(row, Progress), index(row, Speed),
        {Qt::DisplayRole, Qt::UserRole});
}

void ComputeTransferModel::complete_transfer(const QString& id) {
    const int row = find_row(id);
    if (row < 0) return;
    auto& t = transfers_[static_cast<std::size_t>(row)];
    t.progress = 100;
    t.speed    = {};
    t.status   = tr("Complete");
    emit dataChanged(index(row, Progress), index(row, Status),
        {Qt::DisplayRole, Qt::UserRole});
}

void ComputeTransferModel::fail_transfer(
    const QString& id, const QString& reason) {
    const int row = find_row(id);
    if (row < 0) return;
    auto& t = transfers_[static_cast<std::size_t>(row)];
    t.speed  = {};
    t.status = reason.isEmpty() ? tr("Failed") : tr("Failed: %1").arg(reason);
    emit dataChanged(index(row, Speed), index(row, Status),
        {Qt::DisplayRole});
}

void ComputeTransferModel::clear_finished() {
    // Walk backwards to avoid index shifting.
    for (int row = static_cast<int>(transfers_.size()) - 1; row >= 0; --row) {
        const auto& t = transfers_[static_cast<std::size_t>(row)];
        if (t.progress == 100 || t.status.startsWith(tr("Failed"))) {
            beginRemoveRows({}, row, row);
            transfers_.erase(transfers_.begin() + row);
            endRemoveRows();
        }
    }
}

int ComputeTransferModel::find_row(const QString& id) const {
    for (int i = 0; i < static_cast<int>(transfers_.size()); ++i) {
        if (transfers_[static_cast<std::size_t>(i)].id == id)
            return i;
    }
    return -1;
}

} // namespace ores::qt
