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
#include "ores.qt/BookHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ui_BookHistoryDialog.h"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

BookHistoryDialog::BookHistoryDialog(const boost::uuids::uuid& id,
                                     const QString& code,
                                     ClientManager* clientManager,
                                     QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::BookHistoryDialog)
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

BookHistoryDialog::~BookHistoryDialog() {
    delete ui_;
}

QString BookHistoryDialog::code() const {
    return code_;
}

void BookHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), debug) << "Loading history for book: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    refdata::messaging::get_book_history_request request;
    request.id = boost::uuids::to_string(id_);

    QPointer<BookHistoryDialog> self = this;
    runHistoryRequest(clientManager_,
                      std::move(request),
                      [self](refdata::messaging::get_book_history_response response) {
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

int BookHistoryDialog::historySize() const {
    return static_cast<int>(versions_.size());
}

QString BookHistoryDialog::historyTitle() const {
    return QString("History for: %1").arg(code_);
}

HistoryDialogBase::VersionRow BookHistoryDialog::versionRow(int index) const {
    const auto& v = versions_[index];
    return {v.version,
            {relative_time_helper::format(v.recorded_at),
             QString::fromStdString(v.modified_by),
             QString::fromStdString(v.performed_by),
             QString::fromStdString(v.change_commentary)}};
}

HistoryDialogBase::DiffResult BookHistoryDialog::calculateDiffAt(int ci, int pi) const {
    DiffResult diffs;
    const auto& curr = versions_[ci];
    const auto& prev = versions_[pi];

    if (curr.id != prev.id)
        diffs.append({tr("Id"),
                      {QString::fromStdString(boost::uuids::to_string(prev.id)),
                       QString::fromStdString(boost::uuids::to_string(curr.id))}});
    checkString(diffs, tr("Name"), curr.name, prev.name);
    checkString(diffs, tr("Ledger Currency"), curr.ledger_ccy, prev.ledger_ccy);
    checkString(diffs, tr("GL Account Ref"), curr.gl_account_ref, prev.gl_account_ref);
    checkString(diffs, tr("Cost Center"), curr.cost_center, prev.cost_center);
    checkString(diffs, tr("Status"), curr.book_status, prev.book_status);
    checkBool(diffs, tr("Trading Book"), curr.is_trading_book, prev.is_trading_book);
    return diffs;
}

void BookHistoryDialog::displayFullDetails(int index) {
    if (index < 0 || static_cast<size_t>(index) >= versions_.size())
        return;

    const auto& version = versions_[index];

    ui_->idValue->setText(QString::fromStdString(boost::uuids::to_string(version.id)));
    ui_->nameValue->setText(QString::fromStdString(version.name));
    ui_->ledgerCcyValue->setText(QString::fromStdString(version.ledger_ccy));
    ui_->glAccountRefValue->setText(QString::fromStdString(version.gl_account_ref));
    ui_->costCenterValue->setText(QString::fromStdString(version.cost_center));
    ui_->bookStatusValue->setText(QString::fromStdString(version.book_status));
    ui_->isTradingBookCheck->setText(version.is_trading_book ? tr("true") : tr("false"));
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(version.change_commentary));
}

void BookHistoryDialog::openVersionAt(int index) {
    emit openVersionRequested(versions_[index], versions_[index].version);
}

void BookHistoryDialog::revertToVersionAt(int index) {
    emit revertVersionRequested(versions_[index]);
}

}
