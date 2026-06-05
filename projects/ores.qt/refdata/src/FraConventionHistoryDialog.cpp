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
#include "ores.qt/FraConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/fra_convention_protocol.hpp"
#include "ui_FraConventionHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

FraConventionHistoryDialog::FraConventionHistoryDialog(const QString& code,
                                                       ClientManager* clientManager,
                                                       QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::FraConventionHistoryDialog)
    , code_(code)
    , clientManager_(clientManager) {

    ui_->setupUi(this);

    ui_->titleLabel->setText(QString("History for: %1").arg(code_));

    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Performed By", "Commentary"});

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

FraConventionHistoryDialog::~FraConventionHistoryDialog() = default;

void FraConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for FRA convention: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_fra_convention_history_request request;
    request.id = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.fra_conventions);
        historyLoaded();
    });
}

int FraConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow
FraConventionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString FraConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
FraConventionHistoryDialog::calculateDiffAt(int current_index,
                                            int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Id", current.id, previous.id);
    checkString(diffs, "Index", current.index, previous.index);

    return diffs;
}

void FraConventionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->indexValue->setText(QString::fromStdString(version.index));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(version.change_commentary));
}

void FraConventionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening FRA convention version "
                              << version.version << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void FraConventionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.version;

    emit revertVersionRequested(selected);
}

}
