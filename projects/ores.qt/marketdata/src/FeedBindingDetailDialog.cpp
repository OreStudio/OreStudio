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
#include "ores.qt/FeedBindingDetailDialog.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_FeedBindingDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FeedBindingDetailDialog::FeedBindingDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::FeedBindingDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

FeedBindingDetailDialog::~FeedBindingDetailDialog() {
    delete ui_;
}

QTabWidget* FeedBindingDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FeedBindingDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FeedBindingDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FeedBindingDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FeedBindingDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &FeedBindingDetailDialog::onCloseClicked);

    connect(
        ui_->oreKeyEdit, &QLineEdit::textChanged, this, &FeedBindingDetailDialog::onCodeChanged);
    connect(
        ui_->sourceEdit, &QLineEdit::textChanged, this, &FeedBindingDetailDialog::onFieldChanged);
}

void FeedBindingDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FeedBindingDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FeedBindingDetailDialog::setBinding(
    const ores::marketdata::domain::feed_binding& feed_binding) {
    feed_binding_ = feed_binding;
    updateUiFromBinding();
}

void FeedBindingDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        feed_binding_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->oreKeyEdit->setReadOnly(true);
    ui_->sourceEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void FeedBindingDetailDialog::updateUiFromBinding() {
    ui_->oreKeyEdit->setText(QString::fromStdString(feed_binding_.ore_key));
    ui_->sourceEdit->setText(QString::fromStdString(feed_binding_.source_name));
    ui_->enabledEdit->setChecked(feed_binding_.enabled);

    populateProvenance(feed_binding_.version,
                       feed_binding_.modified_by,
                       feed_binding_.performed_by,
                       feed_binding_.recorded_at,
                       feed_binding_.change_reason_code,
                       feed_binding_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::updateBindingFromUi() {
    if (createMode_) {
        feed_binding_.ore_key = ui_->oreKeyEdit->text().trimmed().toStdString();
    }
    feed_binding_.source_name = ui_->sourceEdit->text().trimmed().toStdString();
    feed_binding_.enabled = ui_->enabledEdit->isChecked();
    feed_binding_.modified_by = username_;
}

void FeedBindingDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FeedBindingDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool FeedBindingDetailDialog::validateInput() {
    const QString ore_key_val = ui_->oreKeyEdit->text().trimmed();
    const QString source_name_val = ui_->sourceEdit->text().trimmed();

    return true && !ore_key_val.isEmpty() && !source_name_val.isEmpty();
}

void FeedBindingDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save feed binding while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    feed_binding_.change_reason_code = crSel->reason_code;
    feed_binding_.change_commentary = crSel->commentary;

    updateBindingFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving feed binding: " << feed_binding_.ore_key;

    QPointer<FeedBindingDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, feed_binding = feed_binding_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        marketdata::messaging::save_feed_binding_request request;
        request.data = feed_binding;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Feed Binding saved successfully";
            QString code = QString::fromStdString(self->feed_binding_.ore_key);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->feed_bindingSaved(code);
            self->notifySaveSuccess(tr("Feed Binding '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void FeedBindingDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete feed binding while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(feed_binding_.ore_key);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Feed Binding",
        QString("Are you sure you want to delete feed binding '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting feed binding: " << feed_binding_.ore_key;

    QPointer<FeedBindingDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(feed_binding_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        marketdata::messaging::delete_feed_binding_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Feed Binding deleted successfully";
            emit self->statusMessage(QString("Feed Binding '%1' deleted").arg(code));
            emit self->feed_bindingDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
