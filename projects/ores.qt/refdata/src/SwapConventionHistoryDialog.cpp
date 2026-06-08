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
#include "ores.qt/SwapConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/swap_convention_protocol.hpp"
#include "ui_SwapConventionHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return value ? QString::fromStdString(*value) : QString{};
}

}

SwapConventionHistoryDialog::SwapConventionHistoryDialog(const QString& code,
                                                         ClientManager* clientManager,
                                                         QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::SwapConventionHistoryDialog)
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

SwapConventionHistoryDialog::~SwapConventionHistoryDialog() = default;

void SwapConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for swap convention: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_swap_convention_history_request request;
    request.id = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.swap_conventions);
        historyLoaded();
    });
}

int SwapConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow SwapConventionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString SwapConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
SwapConventionHistoryDialog::calculateDiffAt(int current_index, int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Id", current.id, previous.id);
    checkString(diffs, "Fixed Frequency", current.fixed_frequency, previous.fixed_frequency);
    checkString(diffs,
                "Fixed Day Count Fraction",
                current.fixed_day_count_fraction,
                previous.fixed_day_count_fraction);
    checkString(diffs, "Index", current.index, previous.index);
    checkString(diffs, "Fixed Calendar", current.fixed_calendar, previous.fixed_calendar);
    checkString(diffs, "Fixed Convention", current.fixed_convention, previous.fixed_convention);
    checkString(diffs, "Float Frequency", current.float_frequency, previous.float_frequency);
    checkString(diffs,
                "Sub-Periods Coupon Type",
                current.sub_periods_coupon_type,
                previous.sub_periods_coupon_type);

    return diffs;
}

void SwapConventionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->fixedFrequencyValue->setText(QString::fromStdString(version.fixed_frequency));
    ui_->fixedDayCountFractionValue->setText(
        QString::fromStdString(version.fixed_day_count_fraction));
    ui_->indexValue->setText(QString::fromStdString(version.index));
    ui_->fixedCalendarValue->setText(optionalText(version.fixed_calendar));
    ui_->fixedConventionValue->setText(optionalText(version.fixed_convention));
    ui_->floatFrequencyValue->setText(optionalText(version.float_frequency));
    ui_->subPeriodsCouponTypeValue->setText(optionalText(version.sub_periods_coupon_type));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void SwapConventionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening swap convention version " << version.version
                              << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void SwapConventionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning, so simply request the revert to the selected version.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version;

    emit revertVersionRequested(selected);
}

}
