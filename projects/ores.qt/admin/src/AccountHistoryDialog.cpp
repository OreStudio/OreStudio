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
#include "ores.qt/AccountHistoryDialog.hpp"
#include "ores.iam.api/messaging/protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

AccountHistoryDialog::AccountHistoryDialog(QString username,
                                           ClientManager* clientManager,
                                           QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::AccountHistoryDialog)
    , clientManager_(clientManager)
    , username_(std::move(username)) {

    BOOST_LOG_SEV(lg(), info) << "Creating account history widget for: "
                              << username_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

void AccountHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading account history for: "
                              << username_.toStdString();

    iam::messaging::get_account_history_request request;
    request.username = username_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        history_ = std::move(response.history);
        historyLoaded();
    });
}

int AccountHistoryDialog::historySize() const {
    return static_cast<int>(history_.versions.size());
}

HistoryDialogBase::VersionRow AccountHistoryDialog::versionRow(int index) const {
    const auto& version = history_.versions[index];
    return {.version = version.version_number,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by)}};
}

QString AccountHistoryDialog::historyTitle() const {
    const auto& latest = history_.versions.front();
    return QString("Account History: %1")
        .arg(QString::fromStdString(latest.data.username));
}

HistoryDialogBase::DiffResult
AccountHistoryDialog::calculateDiffAt(int current_index,
                                      int previous_index) const {
    const auto& current = history_.versions[current_index].data;
    const auto& previous = history_.versions[previous_index].data;

    DiffResult diffs;
    checkString(diffs, "Username", current.username, previous.username);
    checkString(diffs, "Email", current.email, previous.email);

    return diffs;
}

void AccountHistoryDialog::displayFullDetails(int index) {
    const auto& version = history_.versions[index];
    const auto& data = version.data;

    ui_->usernameValue->setText(QString::fromStdString(data.username));
    ui_->emailValue->setText(QString::fromStdString(data.email));
    // Hide the isAdmin row in the UI
    ui_->isAdminLabel->setVisible(false);
    ui_->isAdminValue->setVisible(false);
    ui_->versionNumberValue->setText(QString::number(version.version_number));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(version.recorded_at));
}

void AccountHistoryDialog::openVersionAt(int index) {
    const auto& version = history_.versions[index];
    BOOST_LOG_SEV(lg(), info) << "Opening account version "
                              << version.version_number
                              << " in read-only mode";
    emit openVersionRequested(version.data, version.version_number);
}

void AccountHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the version
    // older than the selection, stamped with the latest version number.
    const auto& previous = history_.versions[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << previous.version_number;

    iam::domain::account account = previous.data;
    account.version = history_.versions.front().version_number;
    emit revertVersionRequested(account);
}

}
