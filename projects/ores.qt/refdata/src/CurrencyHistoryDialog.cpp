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
#include "ores.qt/CurrencyHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ui_CurrencyHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

CurrencyHistoryDialog::CurrencyHistoryDialog(const QString& code,
                                             ClientManager* clientManager,
                                             QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CurrencyHistoryDialog)
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

CurrencyHistoryDialog::~CurrencyHistoryDialog() {
    delete ui_;
}

QString CurrencyHistoryDialog::code() const {
    return code_;
}

void CurrencyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for currency: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_currency_history_request request;
    request.iso_code = code_.toStdString();

    QPointer<CurrencyHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](refdata::messaging::get_currency_history_response response) {
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

int CurrencyHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString CurrencyHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow CurrencyHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult CurrencyHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("ISO Code"), curr.iso_code, prev.iso_code);
    checkString(diffs, tr("Name"), curr.name, prev.name);
    checkString(diffs, tr("Numeric Code"), curr.numeric_code, prev.numeric_code);
    checkString(diffs, tr("Asset Class"), curr.monetary_nature, prev.monetary_nature);
    checkString(diffs, tr("Market Tier"), curr.market_tier, prev.market_tier);
    checkString(diffs, tr("Symbol"), curr.symbol, prev.symbol);
    checkString(diffs, tr("Fraction Symbol"), curr.fraction_symbol, prev.fraction_symbol);
    checkInt(diffs, tr("Fractions Per Unit"), curr.fractions_per_unit, prev.fractions_per_unit);
    checkString(diffs, tr("Format"), curr.format, prev.format);
    checkString(diffs, tr("Rounding Type"), curr.rounding_type, prev.rounding_type);
    checkInt(diffs, tr("Rounding Precision"), curr.rounding_precision, prev.rounding_precision);
    return diffs;
}

void CurrencyHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->isoCodeValue->setText(QString::fromStdString(version.iso_code));
    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->numericCodeValue->setText(QString::fromStdString(version.numeric_code));
    ui_->monetaryNatureValue->setText(QString::fromStdString(version.monetary_nature));
    ui_->marketTierValue->setText(QString::fromStdString(version.market_tier));
    ui_->symbolValue->setText(QString::fromStdString(version.symbol));
    ui_->fractionSymbolValue->setText(QString::fromStdString(version.fraction_symbol));
    ui_->fractionsPerUnitSpinBox->setText(QString::number(version.fractions_per_unit));
    ui_->formatValue->setText(QString::fromStdString(version.format));
    ui_->roundingTypeValue->setText(QString::fromStdString(version.rounding_type));
    ui_->roundingPrecisionSpinBox->setText(QString::number(version.rounding_precision));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void CurrencyHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void CurrencyHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
