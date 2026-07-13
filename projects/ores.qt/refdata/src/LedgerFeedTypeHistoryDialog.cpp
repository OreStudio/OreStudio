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
#include "ores.qt/LedgerFeedTypeHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/ledger_feed_type_protocol.hpp"
#include "ui_LedgerFeedTypeHistoryDialog.h"

namespace ores::qt {

using namespace ores::logging;

LedgerFeedTypeHistoryDialog::LedgerFeedTypeHistoryDialog(const QString& code,
                                                         ClientManager* clientManager,
                                                         QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::LedgerFeedTypeHistoryDialog)
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

LedgerFeedTypeHistoryDialog::~LedgerFeedTypeHistoryDialog() {
    delete ui_;
}

QString LedgerFeedTypeHistoryDialog::code() const {
    return code_;
}

void LedgerFeedTypeHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for ledger feed type: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_ledger_feed_type_history_request request;
    request.code = code_.toStdString();

    QPointer<LedgerFeedTypeHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](refdata::messaging::get_ledger_feed_type_history_response response) {
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

int LedgerFeedTypeHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString LedgerFeedTypeHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow LedgerFeedTypeHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult LedgerFeedTypeHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    checkString(diffs, tr("Code"), curr.code, prev.code);
    checkString(diffs, tr("Name"), curr.name, prev.name);
    checkString(diffs, tr("Description"), curr.description, prev.description);
    return diffs;
}

void LedgerFeedTypeHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->codeValue->setText(QString::fromStdString(version.code));
    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->descriptionValue->setText(QString::fromStdString(version.description));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void LedgerFeedTypeHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void LedgerFeedTypeHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
