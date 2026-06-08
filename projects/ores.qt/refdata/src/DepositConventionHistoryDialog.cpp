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
#include "ores.qt/DepositConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/deposit_convention_protocol.hpp"
#include "ui_DepositConventionHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return value ? QString::fromStdString(*value) : QString{};
}

QString optionalBoolText(const std::optional<bool>& value) {
    return value ? (*value ? QObject::tr("true") : QObject::tr("false")) : QObject::tr("(unset)");
}

QString optionalIntText(const std::optional<int>& value) {
    return value ? QString::number(*value) : QObject::tr("(unset)");
}

}

DepositConventionHistoryDialog::DepositConventionHistoryDialog(const QString& code,
                                                               ClientManager* clientManager,
                                                               QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::DepositConventionHistoryDialog)
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

DepositConventionHistoryDialog::~DepositConventionHistoryDialog() = default;

void DepositConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for deposit convention: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_deposit_convention_history_request request;
    request.id = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.deposit_conventions);
        historyLoaded();
    });
}

int DepositConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow DepositConventionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString DepositConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
DepositConventionHistoryDialog::calculateDiffAt(int current_index, int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Id", current.id, previous.id);
    checkBool(diffs, "Index Based", current.index_based, previous.index_based);
    checkString(diffs, "Index", current.index, previous.index);
    checkString(diffs, "Calendar", current.calendar, previous.calendar);
    checkString(diffs, "Convention", current.convention, previous.convention);
    checkString(
        diffs, "Day Count Fraction", current.day_count_fraction, previous.day_count_fraction);

    if (current.end_of_month != previous.end_of_month) {
        diffs.append(
            {"End Of Month",
             {optionalBoolText(previous.end_of_month), optionalBoolText(current.end_of_month)}});
    }

    if (current.settlement_days != previous.settlement_days) {
        diffs.append({"Settlement Days",
                      {optionalIntText(previous.settlement_days),
                       optionalIntText(current.settlement_days)}});
    }

    return diffs;
}

void DepositConventionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->indexBasedValue->setText(version.index_based ? tr("true") : tr("false"));
    ui_->indexValue->setText(optionalText(version.index));
    ui_->calendarValue->setText(optionalText(version.calendar));
    ui_->conventionValue->setText(optionalText(version.convention));
    ui_->dayCountFractionValue->setText(optionalText(version.day_count_fraction));
    ui_->endOfMonthValue->setText(optionalBoolText(version.end_of_month));
    ui_->settlementDaysValue->setText(optionalIntText(version.settlement_days));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void DepositConventionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening deposit convention version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void DepositConventionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
