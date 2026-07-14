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
#include "ores.qt/IrCurveGenerationConfigHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ui_IrCurveGenerationConfigHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

IrCurveGenerationConfigHistoryDialog::IrCurveGenerationConfigHistoryDialog(
    const boost::uuids::uuid& id,
    const QString& code,
    ClientManager* clientManager,
    QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::IrCurveGenerationConfigHistoryDialog)
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

IrCurveGenerationConfigHistoryDialog::~IrCurveGenerationConfigHistoryDialog() {
    delete ui_;
}

QString IrCurveGenerationConfigHistoryDialog::code() const {
    return code_;
}

void IrCurveGenerationConfigHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for IR curve generation config: "
                               << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    synthetic::messaging::get_ir_curve_generation_config_history_request request;
    request.id = boost::uuids::to_string(id_);

    QPointer<IrCurveGenerationConfigHistoryDialog> self = this;
    runHistoryRequest(
        clientManager_,
        std::move(request),
        [self](synthetic::messaging::get_ir_curve_generation_config_history_response response) {
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

int IrCurveGenerationConfigHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString IrCurveGenerationConfigHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow IrCurveGenerationConfigHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult IrCurveGenerationConfigHistoryDialog::calculateDiffAt(int ci,
                                                                                    int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Currency"), curr.currency_code, prev.currency_code);
    checkString(diffs, tr("Index"), curr.index_name, prev.index_name);
    checkString(diffs, tr("Process Type"), curr.process_type, prev.process_type);
    checkDouble(diffs, tr("Kappa"), curr.kappa, prev.kappa);
    checkDouble(diffs, tr("Theta"), curr.theta, prev.theta);
    checkDouble(diffs, tr("Sigma"), curr.sigma, prev.sigma);
    checkDouble(diffs, tr("Initial Rate"), curr.initial_rate, prev.initial_rate);
    checkInt(diffs, tr("Ticks per Hour"), curr.ticks_per_hour, prev.ticks_per_hour);
    checkBool(diffs, tr("Enabled"), curr.enabled, prev.enabled);
    return diffs;
}

void IrCurveGenerationConfigHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->currencyValue->setText(QString::fromStdString(version.currency_code));
    ui_->indexNameValue->setText(QString::fromStdString(version.index_name));
    ui_->processTypeValue->setText(QString::fromStdString(version.process_type));
    ui_->kappaValue->setText(QString::number(version.kappa));
    ui_->thetaValue->setText(QString::number(version.theta));
    ui_->sigmaValue->setText(QString::number(version.sigma));
    ui_->initialRateValue->setText(QString::number(version.initial_rate));
    ui_->ticksPerHourValue->setText(QString::number(version.ticks_per_hour));
    ui_->enabledCheck->setText(version.enabled ? tr("true") : tr("false"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void IrCurveGenerationConfigHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void IrCurveGenerationConfigHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
