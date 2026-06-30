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
#include "ores.qt/FxSpotGenerationConfigHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ui_FxSpotGenerationConfigHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxSpotGenerationConfigHistoryDialog::FxSpotGenerationConfigHistoryDialog(
    const boost::uuids::uuid& id,
    const QString& code,
    ClientManager* clientManager,
    QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::FxSpotGenerationConfigHistoryDialog)
    , id_(id)
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

FxSpotGenerationConfigHistoryDialog::~FxSpotGenerationConfigHistoryDialog() {
    delete ui_;
}

QString FxSpotGenerationConfigHistoryDialog::code() const {
    return code_;
}

void FxSpotGenerationConfigHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for FX spot generation config: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    synthetic::messaging::get_fx_spot_generation_config_history_request request;
    request.id = boost::uuids::to_string(id_);

    QPointer<FxSpotGenerationConfigHistoryDialog> self = this;
    runHistoryRequest(
        clientManager_,
        std::move(request),
        [self](synthetic::messaging::get_fx_spot_generation_config_history_response response) {
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

int FxSpotGenerationConfigHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString FxSpotGenerationConfigHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow FxSpotGenerationConfigHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult FxSpotGenerationConfigHistoryDialog::calculateDiffAt(int ci,
                                                                                   int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Base Currency"), curr.base_currency_code, prev.base_currency_code);
    checkString(diffs, tr("Quote Currency"), curr.quote_currency_code, prev.quote_currency_code);
    checkDouble(diffs, tr("Initial Price"), curr.gmm_initial_price, prev.gmm_initial_price);
    checkInt(diffs, tr("Ticks per Hour"), curr.ticks_per_hour, prev.ticks_per_hour);
    checkString(diffs, tr("Process Type"), curr.process_type, prev.process_type);
    checkBool(diffs, tr("Enabled"), curr.enabled, prev.enabled);
    return diffs;
}

void FxSpotGenerationConfigHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->baseCurrencyValue->setText(QString::fromStdString(version.base_currency_code));
    ui_->quoteCurrencyValue->setText(QString::fromStdString(version.quote_currency_code));
    ui_->gmmInitialPriceValue->setText(QString::number(version.gmm_initial_price));
    ui_->ticksPerHourValue->setText(QString::number(version.ticks_per_hour));
    ui_->processTypeValue->setText(QString::fromStdString(version.process_type));
    ui_->enabledCheck->setText(version.enabled ? tr("true") : tr("false"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void FxSpotGenerationConfigHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void FxSpotGenerationConfigHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
