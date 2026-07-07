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
#include "ores.qt/CurrencyPairDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CurrencyPairDetailDialog::CurrencyPairDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyPairDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupCombos();
    setupConnections();
}

CurrencyPairDetailDialog::~CurrencyPairDetailDialog() {
    delete ui_;
}

QTabWidget* CurrencyPairDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CurrencyPairDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CurrencyPairDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CurrencyPairDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CurrencyPairDetailDialog::setupCombos() {}

void CurrencyPairDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CurrencyPairDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &CurrencyPairDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &CurrencyPairDetailDialog::onCloseClicked);

    connect(
        ui_->pairCodeEdit, &QLineEdit::textChanged, this, &CurrencyPairDetailDialog::onCodeChanged);
    connect(ui_->baseCurrencyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
    connect(ui_->quoteCurrencyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
    connect(ui_->settlementCurrencyCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
    connect(ui_->classificationEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
    connect(ui_->fixingSourceEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
}

void CurrencyPairDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateBaseCurrencyCombo();
    populateQuoteCurrencyCombo();
    populateSettlementCurrencyCombo();
}

void CurrencyPairDetailDialog::populateBaseCurrencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CurrencyPairDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->baseCurrencyCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setPair() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = QString::fromStdString(self->pair_.base_currency);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(combo, self->imageCache(), FlagSource::Currency);
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void CurrencyPairDetailDialog::populateQuoteCurrencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CurrencyPairDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->quoteCurrencyCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setPair() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = QString::fromStdString(self->pair_.quote_currency);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(combo, self->imageCache(), FlagSource::Currency);
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void CurrencyPairDetailDialog::populateSettlementCurrencyCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CurrencyPairDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->settlementCurrencyCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            combo->addItem(QString());
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setPair() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = self->pair_.settlement_currency ?
                                         QString::fromStdString(*self->pair_.settlement_currency) :
                                         QString{};
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(combo, self->imageCache(), FlagSource::Currency);
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void CurrencyPairDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyPairDetailDialog::setPair(const refdata::domain::currency_pair& pair) {
    pair_ = pair;
    updateUiFromPair();
}

void CurrencyPairDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->pairCodeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->pairCodeEdit->setReadOnly(true);
    ui_->baseCurrencyCombo->setEnabled(!readOnly);
    ui_->quoteCurrencyCombo->setEnabled(!readOnly);
    ui_->settlementCurrencyCombo->setEnabled(!readOnly);
    ui_->classificationEdit->setReadOnly(readOnly);
    ui_->fixingSourceEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CurrencyPairDetailDialog::updateUiFromPair() {
    ui_->pairCodeEdit->setText(QString::fromStdString(pair_.pair_code));
    {
        const auto val = QString::fromStdString(pair_.base_currency);
        const int idx = ui_->baseCurrencyCombo->findText(val);
        ui_->baseCurrencyCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(pair_.quote_currency);
        const int idx = ui_->quoteCurrencyCombo->findText(val);
        ui_->quoteCurrencyCombo->setCurrentIndex(idx);
    }
    ui_->deliverableCheckBox->setChecked(pair_.deliverable);
    {
        const auto val = pair_.settlement_currency ?
                             QString::fromStdString(*pair_.settlement_currency) :
                             QString{};
        const int idx = ui_->settlementCurrencyCombo->findText(val);
        ui_->settlementCurrencyCombo->setCurrentIndex(idx);
    }
    ui_->classificationEdit->setText(QString::fromStdString(pair_.classification));
    ui_->fixingSourceEdit->setText(
        pair_.fixing_source ? QString::fromStdString(*pair_.fixing_source) : QString{});

    populateProvenance(pair_.version,
                       pair_.modified_by,
                       pair_.performed_by,
                       pair_.recorded_at,
                       pair_.change_reason_code,
                       pair_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::updatePairFromUi() {
    if (createMode_) {
        pair_.pair_code = ui_->pairCodeEdit->text().trimmed().toStdString();
    }
    pair_.base_currency = ui_->baseCurrencyCombo->currentText().toStdString();
    pair_.quote_currency = ui_->quoteCurrencyCombo->currentText().toStdString();
    pair_.deliverable = ui_->deliverableCheckBox->isChecked();
    {
        const auto _txt = ui_->settlementCurrencyCombo->currentText().trimmed().toStdString();
        pair_.settlement_currency = _txt.empty() ? std::nullopt : std::optional<std::string>(_txt);
    }
    pair_.classification = ui_->classificationEdit->text().trimmed().toStdString();
    {
        const auto fixing_source_str = ui_->fixingSourceEdit->text().trimmed().toStdString();
        pair_.fixing_source = fixing_source_str.empty() ?
                                  std::nullopt :
                                  std::optional<std::string>(fixing_source_str);
    }
    pair_.modified_by = username_;
}

void CurrencyPairDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CurrencyPairDetailDialog::validateInput() {
    const QString pair_code_val = ui_->pairCodeEdit->text().trimmed();
    const QString classification_val = ui_->classificationEdit->text().trimmed();
    const bool base_currency_selected = ui_->baseCurrencyCombo->currentIndex() >= 0;
    const bool quote_currency_selected = ui_->quoteCurrencyCombo->currentIndex() >= 0;

    return true && !pair_code_val.isEmpty() && !classification_val.isEmpty() &&
           base_currency_selected && quote_currency_selected;
}

void CurrencyPairDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save currency pair while disconnected from server.");
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
    pair_.change_reason_code = crSel->reason_code;
    pair_.change_commentary = crSel->commentary;

    updatePairFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving currency pair: " << pair_.pair_code;

    QPointer<CurrencyPairDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, pair = pair_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_currency_pair_request request;
        request.data = pair;
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
            BOOST_LOG_SEV(lg(), info) << "Currency Pair saved successfully";
            QString code = QString::fromStdString(self->pair_.pair_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->pairSaved(code);
            self->notifySaveSuccess(tr("Currency Pair '%1' saved").arg(code));
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

void CurrencyPairDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete currency pair while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(pair_.pair_code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Currency Pair",
        QString("Are you sure you want to delete currency pair '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting currency pair: " << pair_.pair_code;

    QPointer<CurrencyPairDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = pair_.pair_code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_currency_pair_request request;
        request.pair_codes = {code};
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
            BOOST_LOG_SEV(lg(), info) << "Currency Pair deleted successfully";
            emit self->statusMessage(QString("Currency Pair '%1' deleted").arg(code));
            emit self->pairDeleted(code);
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
