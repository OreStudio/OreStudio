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
#include "ores.qt/CountryHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ui_CountryHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

CountryHistoryDialog::CountryHistoryDialog(const QString& code,
                                           ClientManager* clientManager,
                                           QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CountryHistoryDialog)
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

CountryHistoryDialog::~CountryHistoryDialog() {
    delete ui_;
}

QString CountryHistoryDialog::code() const {
    return code_;
}

void CountryHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for country: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_country_history_request request;
    request.alpha2_code = code_.toStdString();

    QPointer<CountryHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](refdata::messaging::get_country_history_response response) {
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

int CountryHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString CountryHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow CountryHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult CountryHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Alpha-2 Code"), curr.alpha2_code, prev.alpha2_code);
    checkString(diffs, tr("Alpha-3 Code"), curr.alpha3_code, prev.alpha3_code);
    checkString(diffs, tr("Numeric Code"), curr.numeric_code, prev.numeric_code);
    checkString(diffs, tr("Name"), curr.name, prev.name);
    checkString(diffs, tr("Official Name"), curr.official_name, prev.official_name);
    return diffs;
}

void CountryHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.alpha2_code));
    ui_->alpha3CodeValue->setText(QString::fromStdString(version.alpha3_code));
    ui_->numericCodeValue->setText(QString::fromStdString(version.numeric_code));
    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->officialNameValue->setText(QString::fromStdString(version.official_name));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void CountryHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void CountryHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
