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
#include "ores.qt/CdsConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/cds_convention_protocol.hpp"
#include "ui_CdsConventionHistoryDialog.h"
#include <QHeaderView>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString optionalText(const std::optional<std::string>& value) {
    return value ? QString::fromStdString(*value) : QString{};
}

}

CdsConventionHistoryDialog::CdsConventionHistoryDialog(const QString& code,
                                                       ClientManager* clientManager,
                                                       QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CdsConventionHistoryDialog)
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

CdsConventionHistoryDialog::~CdsConventionHistoryDialog() = default;

void CdsConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for CDS convention: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_cds_convention_history_request request;
    request.id = code_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        versions_ = std::move(response.cds_conventions);
        historyLoaded();
    });
}

int CdsConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

HistoryDialogBase::VersionRow
CdsConventionHistoryDialog::versionRow(int index) const {
    const auto& version = versions_[index];
    return {.version = version.version,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.performed_by),
                      QString::fromStdString(version.change_commentary)}};
}

QString CdsConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::DiffResult
CdsConventionHistoryDialog::calculateDiffAt(int current_index,
                                            int previous_index) const {
    const auto& current = versions_[current_index];
    const auto& previous = versions_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Id", current.id, previous.id);
    checkString(diffs, "Calendar", current.calendar, previous.calendar);
    checkString(diffs, "Frequency", current.frequency, previous.frequency);
    checkString(diffs, "Payment Convention", current.payment_convention,
                previous.payment_convention);
    checkString(diffs, "Rule", current.rule, previous.rule);
    checkString(diffs, "Day Count Fraction", current.day_count_fraction,
                previous.day_count_fraction);
    checkInt(diffs, "Settlement Days", current.settlement_days,
             previous.settlement_days);

    if (current.upfront_settlement_days != previous.upfront_settlement_days) {
        diffs.append(
            {"Upfront Settlement Days",
             {previous.upfront_settlement_days ?
                  QString::number(*previous.upfront_settlement_days) : tr("(unset)"),
              current.upfront_settlement_days ?
                  QString::number(*current.upfront_settlement_days) : tr("(unset)")}});
    }

    checkString(diffs, "Last Period DCF", current.last_period_day_count_fraction,
                previous.last_period_day_count_fraction);

    if (current.settles_accrual != previous.settles_accrual) {
        diffs.append({"Settles Accrual",
                      {previous.settles_accrual ? tr("true") : tr("false"),
                       current.settles_accrual ? tr("true") : tr("false")}});
    }

    if (current.pays_at_default_time != previous.pays_at_default_time) {
        diffs.append({"Pays At Default Time",
                      {previous.pays_at_default_time ? tr("true") : tr("false"),
                       current.pays_at_default_time ? tr("true") : tr("false")}});
    }

    return diffs;
}

void CdsConventionHistoryDialog::displayFullDetails(int index) {
    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->calendarValue->setText(QString::fromStdString(version.calendar));
    ui_->frequencyValue->setText(QString::fromStdString(version.frequency));
    ui_->paymentConventionValue->setText(QString::fromStdString(version.payment_convention));
    ui_->ruleValue->setText(QString::fromStdString(version.rule));
    ui_->dayCountFractionValue->setText(QString::fromStdString(version.day_count_fraction));
    ui_->settlementDaysValue->setText(QString::number(version.settlement_days));
    ui_->upfrontSettlementDaysValue->setText(version.upfront_settlement_days ?
                                                 QString::number(*version.upfront_settlement_days) :
                                                 tr("(unset)"));
    ui_->lastPeriodDayCountFractionValue->setText(
        optionalText(version.last_period_day_count_fraction));
    ui_->settlesAccrualValue->setText(version.settles_accrual ? tr("true") : tr("false"));
    ui_->paysAtDefaultTimeValue->setText(version.pays_at_default_time ? tr("true") : tr("false"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void CdsConventionHistoryDialog::openVersionAt(int index) {
    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening CDS convention version "
                              << version.version << " in read-only mode";
    emit openVersionRequested(version, version.version);
}

void CdsConventionHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version. The server handles versioning.
    const auto& selected = versions_[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << selected.version;

    emit revertVersionRequested(selected);
}

}
