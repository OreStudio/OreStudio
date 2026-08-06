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
#include "ores.qt/ChangeReasonHistoryDialog.hpp"
#include "ores.dq.api/messaging/change_reason_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ui_ChangeReasonHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

ChangeReasonHistoryDialog::ChangeReasonHistoryDialog(QString code,
                                                     ClientManager* clientManager,
                                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::ChangeReasonHistoryDialog)
    , clientManager_(clientManager)
    , code_(std::move(code)) {

    BOOST_LOG_SEV(lg(), info) << "Creating change reason history widget for: "
                              << code_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

ChangeReasonHistoryDialog::~ChangeReasonHistoryDialog() = default;

void ChangeReasonHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading change reason history for: " << code_.toStdString();

    dq::messaging::get_change_reason_history_request request;
    request.code = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.history);
        historyLoaded();
    });
}

int ChangeReasonHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow ChangeReasonHistoryDialog::versionRow(int index) const {
    const auto& reason = versions_[index];
    return {.version = reason.version,
            .cells = {relative_time_helper::format(reason.recorded_at),
                      QString::fromStdString(reason.modified_by),
                      QString::fromStdString(reason.change_commentary)}};
}

QString ChangeReasonHistoryDialog::historyTitle() const {
    const auto& latest = versions_.front();
    return QString("Change Reason History: %1").arg(QString::fromStdString(latest.code));
}

HistoryDialogBase::DiffResult ChangeReasonHistoryDialog::calculateDiffAt(int current_index,
                                                                         int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Code", current.code, previous.code);
    checkString(diffs, "Description", current.description, previous.description);
    checkString(diffs, "Category Code", current.category_code, previous.category_code);
    checkBool(diffs, "Applies to Amend", current.applies_to_amend, previous.applies_to_amend);
    checkBool(diffs, "Applies to Delete", current.applies_to_delete, previous.applies_to_delete);
    checkBool(
        diffs, "Requires Commentary", current.requires_commentary, previous.requires_commentary);
    checkInt(diffs, "Display Order", current.display_order, previous.display_order);

    return diffs;
}

void ChangeReasonHistoryDialog::displayFullDetails(int index) {
    const auto& reason = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(reason.code));
    ui_->descriptionValue->setText(QString::fromStdString(reason.description));
    ui_->categoryCodeValue->setText(QString::fromStdString(reason.category_code));
    ui_->appliesToAmendValue->setText(reason.applies_to_amend ? "Yes" : "No");
    ui_->appliesToDeleteValue->setText(reason.applies_to_delete ? "Yes" : "No");
    ui_->requiresCommentaryValue->setText(reason.requires_commentary ? "Yes" : "No");
    ui_->displayOrderValue->setText(QString::number(reason.display_order));
    ui_->versionNumberValue->setText(QString::number(reason.version));
    ui_->modifiedByValue->setText(QString::fromStdString(reason.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(reason.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(reason.change_commentary));
}

void ChangeReasonHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening change reason version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void ChangeReasonHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version, stamped with the latest version number.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    dq::domain::change_reason reason = selected;
    reason.version = versions_.front().version;
    emit revertVersionRequested(reason);
}

}
