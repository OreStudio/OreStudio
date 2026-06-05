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
#include "ores.qt/SystemSettingHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

QString enabledText(const std::string& value) {
    return value == "true" ? QStringLiteral("Yes") : QStringLiteral("No");
}

}

SystemSettingHistoryDialog::SystemSettingHistoryDialog(QString name,
                                                       ClientManager* clientManager,
                                                       QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::SystemSettingHistoryDialog)
    , clientManager_(clientManager)
    , flagName_(std::move(name)) {

    BOOST_LOG_SEV(lg(), info) << "Creating system setting history widget for: "
                              << flagName_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

void SystemSettingHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading system setting history for: "
                              << flagName_.toStdString();

    variability::messaging::get_setting_history_request request;
    request.name = flagName_.toStdString();

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

int SystemSettingHistoryDialog::historySize() const {
    return static_cast<int>(history_.size());
}

HistoryDialogBase::VersionRow
SystemSettingHistoryDialog::versionRow(int index) const {
    const auto& flag = history_[index];
    return {.version = flag.version,
            .cells = {enabledText(flag.value),
                      relative_time_helper::format(flag.recorded_at),
                      QString::fromStdString(flag.modified_by)}};
}

QString SystemSettingHistoryDialog::historyTitle() const {
    const auto& latest = history_.front();
    return QString("System Setting History: %1")
        .arg(QString::fromStdString(latest.name));
}

HistoryDialogBase::DiffResult
SystemSettingHistoryDialog::calculateDiffAt(int current_index,
                                            int previous_index) const {
    const auto& current = history_[current_index];
    const auto& previous = history_[previous_index];

    DiffResult diffs;
    if (current.value != previous.value) {
        diffs.append({"Enabled",
                      {enabledText(previous.value), enabledText(current.value)}});
    }
    checkString(diffs, "Description", current.description,
                previous.description);

    return diffs;
}

void SystemSettingHistoryDialog::displayFullDetails(int index) {
    const auto& flag = history_[index];

    ui_->nameValue->setText(QString::fromStdString(flag.name));
    ui_->enabledValue->setText(enabledText(flag.value));
    ui_->descriptionValue->setText(QString::fromStdString(flag.description));
    ui_->versionNumberValue->setText(QString::number(flag.version));
    ui_->modifiedByValue->setText(QString::fromStdString(flag.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(flag.recorded_at));
}

void SystemSettingHistoryDialog::openVersionAt(int index) {
    const auto& flag = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening system setting version "
                              << flag.version << " in read-only mode";
    emit openVersionRequested(flag, flag.version);
}

void SystemSettingHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version. The server handles versioning - we just send
    // the data we want to restore.
    const auto& selected = history_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.version;

    emit revertVersionRequested(selected);
}

}
