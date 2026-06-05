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
#include "ores.qt/OisConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/ois_convention_protocol.hpp"
#include "ui_OisConventionHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return value ? QString::fromStdString(*value) : QString{};
}

}

OisConventionHistoryDialog::OisConventionHistoryDialog(const QString& code,
                                                       ClientManager* clientManager,
                                                       QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::OisConventionHistoryDialog)
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

OisConventionHistoryDialog::~OisConventionHistoryDialog() = default;

void OisConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for OIS convention: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_ois_convention_history_request request;
    request.id = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.ois_conventions);
        historyLoaded();
    });
}

int OisConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow
OisConventionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString OisConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
OisConventionHistoryDialog::calculateDiffAt(int current_index,
                                            int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Id", current.id, previous.id);
    checkString(diffs, "Index", current.index, previous.index);
    checkInt(diffs, "Spot Lag", current.spot_lag, previous.spot_lag);
    checkString(diffs, "Fixed Day Count Fraction",
                current.fixed_day_count_fraction,
                previous.fixed_day_count_fraction);
    checkString(diffs, "Fixed Calendar", current.fixed_calendar,
                previous.fixed_calendar);
    checkInt(diffs, "Payment Lag", current.payment_lag, previous.payment_lag);
    checkString(diffs, "Fixed Frequency", current.fixed_frequency,
                previous.fixed_frequency);
    checkString(diffs, "Fixed Convention", current.fixed_convention,
                previous.fixed_convention);
    checkString(diffs, "Fixed Payment Convention",
                current.fixed_payment_convention,
                previous.fixed_payment_convention);
    checkString(diffs, "Rule", current.rule, previous.rule);
    checkString(diffs, "Payment Calendar", current.payment_calendar,
                previous.payment_calendar);
    checkInt(diffs, "Rate Cutoff", current.rate_cutoff, previous.rate_cutoff);

    if (current.end_of_month != previous.end_of_month) {
        auto boolText = [this](const std::optional<bool>& v) {
            return v ? (*v ? tr("true") : tr("false")) : tr("(unset)");
        };
        diffs.append({"End Of Month",
                      {boolText(previous.end_of_month),
                       boolText(current.end_of_month)}});
    }

    return diffs;
}

void OisConventionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->indexValue->setText(QString::fromStdString(version.index));
    ui_->spotLagValue->setText(QString::number(version.spot_lag));
    ui_->fixedDayCountFractionValue->setText(
        QString::fromStdString(version.fixed_day_count_fraction));
    ui_->fixedCalendarValue->setText(optionalText(version.fixed_calendar));
    ui_->paymentLagValue->setText(version.payment_lag ?
                                      QString::number(*version.payment_lag) :
                                      tr("(unset)"));
    ui_->fixedFrequencyValue->setText(optionalText(version.fixed_frequency));
    ui_->fixedConventionValue->setText(optionalText(version.fixed_convention));
    ui_->fixedPaymentConventionValue->setText(
        optionalText(version.fixed_payment_convention));
    ui_->ruleValue->setText(optionalText(version.rule));
    ui_->paymentCalendarValue->setText(optionalText(version.payment_calendar));
    ui_->rateCutoffValue->setText(version.rate_cutoff ?
                                      QString::number(*version.rate_cutoff) :
                                      tr("(unset)"));
    ui_->endOfMonthValue->setText(
        version.end_of_month ? (*version.end_of_month ? tr("true") : tr("false")) : tr("(unset)"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(version.change_commentary));
}

void OisConventionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening OIS convention version "
                              << version.version << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void OisConventionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; the server handles
    // versioning, so simply request the revert to the selected version.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.version;

    emit revertVersionRequested(selected);
}

}
