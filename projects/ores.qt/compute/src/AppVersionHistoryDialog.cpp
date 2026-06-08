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
#include "ores.qt/AppVersionHistoryDialog.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ui_AppVersionHistoryDialog.h"
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

AppVersionHistoryDialog::AppVersionHistoryDialog(const boost::uuids::uuid& id,
                                                 const QString& code,
                                                 ClientManager* clientManager,
                                                 QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::AppVersionHistoryDialog)
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

AppVersionHistoryDialog::~AppVersionHistoryDialog() = default;

void AppVersionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for app version: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    compute::messaging::get_app_version_history_request request;
    request.id = boost::uuids::to_string(id_);

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.versions);
        historyLoaded();
    });
}

int AppVersionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow AppVersionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString AppVersionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult AppVersionHistoryDialog::calculateDiffAt(int current_index,
                                                                       int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Wrapper Version", current.wrapper_version, previous.wrapper_version);
    checkString(diffs, "Name", current.wrapper_version, previous.wrapper_version);
    checkString(diffs, "Description", current.engine_version, previous.engine_version);

    return diffs;
}

void AppVersionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.wrapper_version));
    ui_->nameValue->setText(QString::fromStdString(version.engine_version));
    ui_->descriptionValue->setText(QString::fromStdString(version.engine_version));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void AppVersionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening app version version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void AppVersionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version. The server handles versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
