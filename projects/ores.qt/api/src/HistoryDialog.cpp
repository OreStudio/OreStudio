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
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ui_HistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;
using ores::history::messaging::entity_history_version;
using ores::history::messaging::get_entity_history_request;

HistoryDialog::HistoryDialog(std::string entityType,
                             std::string entityId,
                             ClientManager* clientManager,
                             QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::HistoryDialog)
    , entityType_(std::move(entityType))
    , entityId_(std::move(entityId))
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    initializeHistoryUi(
        {ui_->versionListWidget, ui_->changesTableWidget, ui_->titleLabel, ui_->closeButton});
}

HistoryDialog::~HistoryDialog() {
    delete ui_;
}

QString HistoryDialog::code() const {
    return QString::fromStdString(entityId_);
}

void HistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for " << entityType_ << " " << entityId_;
    emit statusChanged(tr("Loading history..."));

    get_entity_history_request request;
    request.entity_type = entityType_;
    request.entity_id = entityId_;

    QPointer<HistoryDialog> self = this;
    runHistoryRequest(clientManager_, std::move(request), [self](auto response) {
        if (!self)
            return;
        if (!response.success) {
            self->historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        self->versions_ = std::move(response.versions);
        self->historyLoaded();
    });
}

int HistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString HistoryDialog::historyTitle() const {
    return tr("History for: %1").arg(QString::fromStdString(entityId_));
}

HistoryDialogBase::VersionRow HistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at), QString::fromStdString(v.modified_by)}};
}

HistoryDialogBase::DiffResult HistoryDialog::calculateDiffAt(int ci, int /*pi*/) const {
    // The server precomputes each version's diff against its own
    // immediate predecessor, which is exactly the pair
    // HistoryDialogBase always requests (previous_index == ci + 1).
    DiffResult diffs;
    for (const auto& entry : versions_[ci].changes.entries) {
        diffs.append(
            {QString::fromStdString(entry.field_name),
             {QString::fromStdString(entry.old_value), QString::fromStdString(entry.new_value)}});
    }
    return diffs;
}

void HistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<std::size_t>(index) >= versions_.size())
        return;

    const auto& fields = versions_[index].fields;
    ui_->fullDetailsTableWidget->setRowCount(static_cast<int>(fields.size()));
    for (int i = 0; i < static_cast<int>(fields.size()); ++i) {
        ui_->fullDetailsTableWidget->setItem(
            i, 0, new QTableWidgetItem(QString::fromStdString(fields[i].name)));
        ui_->fullDetailsTableWidget->setItem(
            i, 1, new QTableWidgetItem(QString::fromStdString(fields[i].value)));
    }
}

void HistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(QString::fromStdString(entityType_),
                              QString::fromStdString(entityId_),
                              versions_[index].version);
}

void HistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(QString::fromStdString(entityType_),
                                QString::fromStdString(entityId_),
                                versions_[index].version);
}

}
