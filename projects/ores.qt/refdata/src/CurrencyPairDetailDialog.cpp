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
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QIcon>
#include <QLineEdit>
#include <QMessageBox>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CurrencyPairDetailDialog::CurrencyPairDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyPairDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
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

QString CurrencyPairDetailDialog::code() const {
    return QString::fromStdString(pair_.pair_code);
}

void CurrencyPairDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // No uploadable flag image of its own — just the derived, read-only
    // inline icon on the key field.
    initKeyFlagField();
}

QLineEdit* CurrencyPairDetailDialog::keyFlagField() const {
    return ui_->pairCodeEdit;
}

QIcon CurrencyPairDetailDialog::keyFlagIcon(const std::string& key) const {
    return imageCache() ? currency_flag_icon_from_pair_code(*imageCache(), key) : QIcon();
}

QSize CurrencyPairDetailDialog::keyFlagIconSize() const {
    return currency_pair_icon_size();
}

void CurrencyPairDetailDialog::setupCombos() {
    ui_->classificationCombo->clear();
    ui_->classificationCombo->addItem(tr("major"), QString("major"));
    ui_->classificationCombo->addItem(tr("minor"), QString("minor"));
    ui_->classificationCombo->addItem(tr("exotic"), QString("exotic"));
    ui_->classificationCombo->addItem(tr("commodity"), QString("commodity"));
}

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
    connect(ui_->classificationCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairDetailDialog::onFieldChanged);
    connect(ui_->baseCurrencyCombo,
            &QComboBox::currentTextChanged,
            this,
            &CurrencyPairDetailDialog::updatePairCodeFromCurrencies);
    connect(ui_->quoteCurrencyCombo,
            &QComboBox::currentTextChanged,
            this,
            &CurrencyPairDetailDialog::updatePairCodeFromCurrencies);
}

void CurrencyPairDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    setup_badge_combo(this, ui_->classificationCombo, badgeCache(), "currency_pair_classification");
    populateBaseCurrencyCombo();
    populateQuoteCurrencyCombo();
}

void CurrencyPairDetailDialog::populateBaseCurrencyCombo() {
    setup_currency_combo(ui_->baseCurrencyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.base_currency);
    });
}

void CurrencyPairDetailDialog::populateQuoteCurrencyCombo() {
    setup_currency_combo(ui_->quoteCurrencyCombo, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(pair_.quote_currency);
    });
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
    ui_->baseCurrencyCombo->setEnabled(createMode);
    ui_->quoteCurrencyCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
    WidgetUtils::set_combo_locked(ui_->baseCurrencyCombo, !createMode);
    WidgetUtils::set_combo_locked(ui_->quoteCurrencyCombo, !createMode);
    // pair_code is always derived (see updatePairCodeFromCurrencies()),
    // never independently typed -- the generated locked_fields loop above
    // left it as setReadOnly(!createMode) (is_key's normal, editable-at-
    // create behaviour), which is wrong for a field that's never directly
    // entered even at create time.
    ui_->pairCodeEdit->setReadOnly(true);
}

void CurrencyPairDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->pairCodeEdit->setReadOnly(true);
    ui_->baseCurrencyCombo->setEnabled(false);
    ui_->quoteCurrencyCombo->setEnabled(false);
    ui_->classificationCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    WidgetUtils::set_combo_locked(ui_->baseCurrencyCombo, true);
    WidgetUtils::set_combo_locked(ui_->quoteCurrencyCombo, true);
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
    {
        const auto val = QString::fromStdString(pair_.classification);
        const int idx = ui_->classificationCombo->findData(val);
        if (idx >= 0)
            ui_->classificationCombo->setCurrentIndex(idx);
    }

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
    if (createMode_) {
        pair_.base_currency = ui_->baseCurrencyCombo->currentText().trimmed().toStdString();
    }
    if (createMode_) {
        pair_.quote_currency = ui_->quoteCurrencyCombo->currentText().trimmed().toStdString();
    }
    pair_.classification = ui_->classificationCombo->currentData().toString().toStdString();
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
    const bool base_currency_selected = ui_->baseCurrencyCombo->currentIndex() >= 0;
    const bool quote_currency_selected = ui_->quoteCurrencyCombo->currentIndex() >= 0;

    return true && !pair_code_val.isEmpty() && base_currency_selected && quote_currency_selected &&
           ui_->baseCurrencyCombo->currentText() != ui_->quoteCurrencyCombo->currentText();
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

    if (createMode_) {
        // pair_.pair_code isn't synced from the UI until updatePairFromUi()
        // runs, later in this function -- read the live combo-derived value
        // directly rather than the (still-stale/empty) domain object.
        const QString baseCode = ui_->baseCurrencyCombo->currentText();
        const QString quoteCode = ui_->quoteCurrencyCombo->currentText();
        const std::string derivedPairCode = ui_->pairCodeEdit->text().trimmed().toStdString();
        const std::string invertedPairCode = (quoteCode + "/" + baseCode).toStdString();

        refdata::messaging::get_currency_pairs_request checkRequest;
        checkRequest.limit = lookup_fetch_limit;
        auto checkResult = clientManager_->process_authenticated_request(std::move(checkRequest));
        if (!checkResult) {
            // Fail closed: the whole point of this check is to prevent a
            // silent overwrite, so if we can't even ask the server whether
            // the pair already exists, don't let the save through blind.
            MessageBoxHelper::warning(
                this,
                "Cannot Verify",
                "Could not check for an existing pair before saving. Please try again.");
            return;
        }

        const bool exists = std::ranges::any_of(
            checkResult->pairs, [&](const auto& p) { return p.pair_code == derivedPairCode; });
        if (exists) {
            MessageBoxHelper::warning(this,
                                      "Duplicate Pair",
                                      QString("A currency pair '%1' already exists.")
                                          .arg(QString::fromStdString(derivedPairCode)));
            return;
        }

        // FX market convention quotes each pair in one canonical
        // direction only -- EUR/USD and USD/EUR are not independent
        // pairs, they're the same market inverted. Reject the inverse
        // rather than silently allowing both to coexist.
        const bool inverseExists = std::ranges::any_of(
            checkResult->pairs, [&](const auto& p) { return p.pair_code == invertedPairCode; });
        if (inverseExists) {
            MessageBoxHelper::warning(
                this,
                "Inverted Pair",
                QString("'%1' is the inverse of the existing pair '%2'. FX market "
                        "convention allows only one direction per currency pair.\n\n"
                        "To use '%1' instead, delete '%2' first, then create '%1'.")
                    .arg(QString::fromStdString(derivedPairCode))
                    .arg(QString::fromStdString(invertedPairCode)));
            return;
        }
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

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
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

void CurrencyPairDetailDialog::updatePairCodeFromCurrencies() {
    if (!createMode_)
        return;
    ui_->pairCodeEdit->setText(ui_->baseCurrencyCombo->currentText() + "/" +
                               ui_->quoteCurrencyCombo->currentText());
}

}
