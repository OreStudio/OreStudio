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
#include "ores.qt/BusinessUnitHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
#include "ui_BusinessUnitHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

BusinessUnitHistoryDialog::BusinessUnitHistoryDialog(const boost::uuids::uuid& id,
                                                     const QString& code,
                                                     ClientManager* clientManager,
                                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::BusinessUnitHistoryDialog)
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

BusinessUnitHistoryDialog::~BusinessUnitHistoryDialog() = default;

void BusinessUnitHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for business unit: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_business_unit_history_request request;
    request.id = boost::uuids::to_string(id_);

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

int BusinessUnitHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow BusinessUnitHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString BusinessUnitHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult BusinessUnitHistoryDialog::calculateDiffAt(int current_index,
                                                                         int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Unit Code", current.unit_code, previous.unit_code);
    checkString(diffs, "Unit Name", current.unit_name, previous.unit_name);
    checkString(
        diffs, "Business Centre", current.business_centre_code, previous.business_centre_code);

    return diffs;
}

void BusinessUnitHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.unit_code));
    ui_->nameValue->setText(QString::fromStdString(version.unit_name));
    ui_->businessCentreValue->setText(QString::fromStdString(version.business_centre_code));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void BusinessUnitHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening business unit version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void BusinessUnitHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
