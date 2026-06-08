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
#include "ores.qt/ChangeReasonCategoryHistoryDialog.hpp"
#include "ores.dq.api/messaging/change_management_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ui_ChangeReasonCategoryHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

ChangeReasonCategoryHistoryDialog::ChangeReasonCategoryHistoryDialog(QString code,
                                                                     ClientManager* clientManager,
                                                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::ChangeReasonCategoryHistoryDialog)
    , clientManager_(clientManager)
    , code_(std::move(code)) {

    BOOST_LOG_SEV(lg(), info) << "Creating category history widget for: " << code_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

ChangeReasonCategoryHistoryDialog::~ChangeReasonCategoryHistoryDialog() = default;

void ChangeReasonCategoryHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading category history for: " << code_.toStdString();

    dq::messaging::get_change_reason_category_history_request request;
    request.code = code_.toStdString();

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

int ChangeReasonCategoryHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow ChangeReasonCategoryHistoryDialog::versionRow(int index) const {
    const auto& category = versions_[index];
    return {.version = category.version,
            .cells = {relative_time_helper::format(category.recorded_at),
                      QString::fromStdString(category.modified_by),
                      QString::fromStdString(category.change_commentary)}};
}

QString ChangeReasonCategoryHistoryDialog::historyTitle() const {
    const auto& latest = versions_.front();
    return QString("Category History: %1").arg(QString::fromStdString(latest.code));
}

HistoryDialogBase::DiffResult
ChangeReasonCategoryHistoryDialog::calculateDiffAt(int current_index, int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Code", current.code, previous.code);
    checkString(diffs, "Description", current.description, previous.description);

    return diffs;
}

void ChangeReasonCategoryHistoryDialog::displayFullDetails(int index) {
    const auto& category = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(category.code));
    ui_->descriptionValue->setText(QString::fromStdString(category.description));
    ui_->versionNumberValue->setText(QString::number(category.version));
    ui_->modifiedByValue->setText(QString::fromStdString(category.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(category.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(category.change_commentary));
}

void ChangeReasonCategoryHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening category version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void ChangeReasonCategoryHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version, stamped with the latest version number.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    dq::domain::change_reason_category category = selected;
    category.version = versions_.front().version;
    emit revertVersionRequested(category);
}

}
