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
#include "ores.qt/WorkspaceHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include "ui_WorkspaceHistoryDialog.h"
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

WorkspaceHistoryDialog::WorkspaceHistoryDialog(const boost::uuids::uuid& id,
                                               const QString& code,
                                               ClientManager* clientManager,
                                               QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::WorkspaceHistoryDialog)
    , id_(id)
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

WorkspaceHistoryDialog::~WorkspaceHistoryDialog() = default;

void WorkspaceHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for workspace: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    workspace::messaging::get_workspace_history_request request;
    request.id = boost::uuids::to_string(id_);

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.workspaces);
        historyLoaded();
    });
}

int WorkspaceHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow
WorkspaceHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString WorkspaceHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
WorkspaceHistoryDialog::calculateDiffAt(int current_index,
                                        int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Name", current.name, previous.name);
    checkString(diffs, "Description", current.description, previous.description);
    checkString(diffs, "Source Path", current.source_path,
                previous.source_path);

    if (current.parent_workspace_id != previous.parent_workspace_id) {
        auto to_s = [](const std::optional<boost::uuids::uuid>& o) {
            return o ? QString::fromStdString(boost::uuids::to_string(*o))
                     : QString{};
        };
        diffs.append({"Parent",
                      {to_s(previous.parent_workspace_id),
                       to_s(current.parent_workspace_id)}});
    }

    if (current.owner_id != previous.owner_id) {
        diffs.append(
            {"Owner",
             {QString::fromStdString(boost::uuids::to_string(previous.owner_id)),
              QString::fromStdString(
                  boost::uuids::to_string(current.owner_id))}});
    }

    checkString(diffs, "Status", current.status_code, previous.status_code);

    return diffs;
}

void WorkspaceHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->sourcePathValue->setText(QString::fromStdString(version.source_path));
    ui_->parentValue->setText(
        version.parent_workspace_id ?
            QString::fromStdString(
                boost::uuids::to_string(*version.parent_workspace_id)) :
            QString{});
    ui_->ownerValue->setText(
        QString::fromStdString(boost::uuids::to_string(version.owner_id)));
    ui_->statusValue->setText(QString::fromStdString(version.status_code));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(version.change_commentary));
}

void WorkspaceHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening workspace version "
                              << version.version << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void WorkspaceHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.version;

    emit revertVersionRequested(selected);
}

}
