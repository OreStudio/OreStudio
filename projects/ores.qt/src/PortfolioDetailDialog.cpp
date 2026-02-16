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
#include "ores.qt/PortfolioDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_PortfolioDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioDetailDialog::PortfolioDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PortfolioDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

PortfolioDetailDialog::~PortfolioDetailDialog() {
    delete ui_;
}

void PortfolioDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void PortfolioDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PortfolioDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PortfolioDetailDialog::onDeleteClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &PortfolioDetailDialog::onCodeChanged);
    connect(ui_->purposeTypeEdit, &QLineEdit::textChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->aggregationCcyEdit, &QLineEdit::textChanged, this,
            &PortfolioDetailDialog::onFieldChanged);
}

void PortfolioDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PortfolioDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PortfolioDetailDialog::setPortfolio(
    const refdata::domain::portfolio& portfolio) {
    portfolio_ = portfolio;
    updateUiFromPortfolio();
}

void PortfolioDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void PortfolioDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->purposeTypeEdit->setReadOnly(readOnly);
    ui_->aggregationCcyEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PortfolioDetailDialog::updateUiFromPortfolio() {
    ui_->nameEdit->setText(QString::fromStdString(portfolio_.name));
    ui_->purposeTypeEdit->setText(QString::fromStdString(portfolio_.purpose_type));
    ui_->aggregationCcyEdit->setText(QString::fromStdString(portfolio_.aggregation_ccy));

    ui_->versionEdit->setText(QString::number(portfolio_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(portfolio_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(portfolio_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(portfolio_.change_commentary));
}

void PortfolioDetailDialog::updatePortfolioFromUi() {
    if (createMode_) {
        portfolio_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    portfolio_.purpose_type = ui_->purposeTypeEdit->text().trimmed().toStdString();
    portfolio_.aggregation_ccy = ui_->aggregationCcyEdit->text().trimmed().toStdString();
    portfolio_.modified_by = username_;
    portfolio_.performed_by = username_;
}

void PortfolioDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PortfolioDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !name_val.isEmpty();
}

void PortfolioDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save portfolio while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updatePortfolioFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, portfolio = portfolio_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_portfolio_request request;
        request.portfolio = portfolio;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_portfolio_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::save_portfolio_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Portfolio saved successfully";
            QString code = QString::fromStdString(self->portfolio_.name);
            emit self->portfolioSaved(code);
            self->notifySaveSuccess(tr("Portfolio '%1' saved").arg(code));
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

void PortfolioDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete portfolio while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(portfolio_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Portfolio",
        QString("Are you sure you want to delete portfolio '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = portfolio_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_portfolio_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_portfolio_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::delete_portfolio_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Portfolio deleted successfully";
            emit self->statusMessage(QString("Portfolio '%1' deleted").arg(code));
            emit self->portfolioDeleted(code);
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
