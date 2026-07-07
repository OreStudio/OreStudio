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
#include "ores.qt/CurrencyPairConventionHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairConventionHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

CurrencyPairConventionHistoryDialog::CurrencyPairConventionHistoryDialog(
    const QString& code, ClientManager* clientManager, QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CurrencyPairConventionHistoryDialog)
    , code_(code)
    , clientManager_(clientManager) {

    ui_->setupUi(this);
    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels({tr("Version"),
                                                       tr("Recorded At"),
                                                       tr("Modified By"),
                                                       tr("Performed By"),
                                                       tr("Commentary")});
    ui_->changesTableWidget->setColumnCount(3);
    ui_->changesTableWidget->setHorizontalHeaderLabels(
        {tr("Field"), tr("Old Value"), tr("New Value")});
    initializeHistoryUi(
        {ui_->versionListWidget, ui_->changesTableWidget, ui_->titleLabel, ui_->closeButton});
}

CurrencyPairConventionHistoryDialog::~CurrencyPairConventionHistoryDialog() {
    delete ui_;
}

QString CurrencyPairConventionHistoryDialog::code() const {
    return code_;
}

void CurrencyPairConventionHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for currency pair convention: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_currency_pair_convention_history_request request;
    request.pair_code = code_.toStdString();

    QPointer<CurrencyPairConventionHistoryDialog> self = this;
    runHistoryRequest(
        clientManager_,
        std::move(request),
        [self](refdata::messaging::get_currency_pair_convention_history_response response) {
            if (!self)
                return;
            if (!response.success) {
                self->historyLoadFailed(QString::fromStdString(response.message));
                return;
            }
            self->versions_ = std::move(response.history);
            self->historyLoaded();
        });
}

int CurrencyPairConventionHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString CurrencyPairConventionHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow CurrencyPairConventionHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult CurrencyPairConventionHistoryDialog::calculateDiffAt(int ci,
                                                                                   int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Pair Code"), curr.pair_code, prev.pair_code);
    checkDouble(diffs, tr("Pip Factor"), curr.pip_factor, prev.pip_factor);
    checkDouble(diffs, tr("Tick Size"), curr.tick_size, prev.tick_size);
    checkInt(diffs, tr("Decimal Places"), curr.decimal_places, prev.decimal_places);
    checkString(diffs, tr("Advance Calendar"), curr.advance_calendar, prev.advance_calendar);
    checkString(diffs,
                tr("Business Day Convention"),
                curr.business_day_convention,
                prev.business_day_convention);
    {
        auto _to_s = [this](const std::optional<bool>& v) {
            return v ? (*v ? tr("true") : tr("false")) : tr("(unset)");
        };
        if (curr.spot_relative != prev.spot_relative)
            diffs.append(
                {tr("Spot Relative"), {_to_s(prev.spot_relative), _to_s(curr.spot_relative)}});
    }
    {
        auto _to_s = [this](const std::optional<bool>& v) {
            return v ? (*v ? tr("true") : tr("false")) : tr("(unset)");
        };
        if (curr.end_of_month != prev.end_of_month)
            diffs.append(
                {tr("End Of Month"), {_to_s(prev.end_of_month), _to_s(curr.end_of_month)}});
    }
    return diffs;
}

void CurrencyPairConventionHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->pairCodeValue->setText(QString::fromStdString(version.pair_code));
    ui_->pipFactorValue->setText(QString::number(version.pip_factor));
    ui_->tickSizeValue->setText(QString::number(version.tick_size));
    ui_->decimalPlacesSpinBox->setText(QString::number(version.decimal_places));
    ui_->advanceCalendarValue->setText(
        version.advance_calendar ? QString::fromStdString(*version.advance_calendar) : QString{});
    ui_->businessDayConventionValue->setText(
        version.business_day_convention ? QString::fromStdString(*version.business_day_convention) :
                                          QString{});
    ui_->spotRelativeCheckBox->setText(version.spot_relative ?
                                           (*version.spot_relative ? tr("true") : tr("false")) :
                                           tr("(unset)"));
    ui_->endOfMonthCheckBox->setText(
        version.end_of_month ? (*version.end_of_month ? tr("true") : tr("false")) : tr("(unset)"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void CurrencyPairConventionHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void CurrencyPairConventionHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
