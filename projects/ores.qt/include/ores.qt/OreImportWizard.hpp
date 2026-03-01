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
#ifndef ORES_QT_ORE_IMPORT_WIZARD_HPP
#define ORES_QT_ORE_IMPORT_WIZARD_HPP

#include <QWizard>
#include <QWizardPage>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QProgressBar>
#include <QTextEdit>
#include <QRadioButton>
#include <QCheckBox>
#include <QListWidget>
#include <QTreeWidget>
#include <QComboBox>
#include <QDateEdit>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.ore/scanner/scan_result.hpp"
#include "ores.ore/planner/import_choices.hpp"
#include "ores.ore/planner/ore_import_plan.hpp"

namespace ores::qt {

/**
 * @brief 7-page wizard for importing ORE directory data into OreStudio.
 *
 * Pages:
 * 1. Welcome      - static explanation
 * 2. Directory    - folder picker; async scan on Next
 * 3. Scan Summary - counts + editable exclusion list; live hierarchy preview
 * 4. Currency     - All vs Missing-only toggle; shows currency count
 * 5. Portfolio    - parent portfolio name / create toggle
 * 6. Trade        - date/lifecycle/counterparty defaults; executes import
 * 7. Done         - summary counts and per-step results
 */
class OreImportWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_import_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_Welcome = 0,
        Page_Directory,
        Page_ScanSummary,
        Page_Currency,
        Page_Portfolio,
        Page_TradeImport,
        Page_Done
    };

    explicit OreImportWizard(ClientManager* clientManager,
                             QWidget* parent = nullptr);
    ~OreImportWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    // Shared state — written by pages, read by subsequent pages
    ore::scanner::scan_result& scanResult() { return scanResult_; }
    ore::planner::import_choices& choices() { return choices_; }
    ore::planner::ore_import_plan& importPlan() { return importPlan_; }

    void setScanResult(ore::scanner::scan_result r) { scanResult_ = std::move(r); }

    // Results set by Page_TradeImport
    int savedCurrencies() const { return savedCurrencies_; }
    int savedPortfolios() const { return savedPortfolios_; }
    int savedBooks() const { return savedBooks_; }
    int savedTrades() const { return savedTrades_; }
    bool importSuccess() const { return importSuccess_; }
    QString importError() const { return importError_; }

    void setImportResults(int currencies, int portfolios, int books, int trades) {
        savedCurrencies_ = currencies;
        savedPortfolios_ = portfolios;
        savedBooks_ = books;
        savedTrades_ = trades;
    }
    void setImportSuccess(bool ok) { importSuccess_ = ok; }
    void setImportError(const QString& msg) { importError_ = msg; }

    // Existing ISO codes fetched on Page_Currency
    const std::set<std::string>& existingIsoCodes() const { return existingIsoCodes_; }
    void setExistingIsoCodes(std::set<std::string> codes) {
        existingIsoCodes_ = std::move(codes);
    }

    // Existing portfolio names fetched on Page_Portfolio
    const std::vector<std::string>& existingPortfolioNames() const {
        return existingPortfolioNames_;
    }
    void setExistingPortfolioNames(std::vector<std::string> names) {
        existingPortfolioNames_ = std::move(names);
    }

private:
    void setupPages();

    ClientManager* clientManager_;
    ore::scanner::scan_result scanResult_;
    ore::planner::import_choices choices_;
    ore::planner::ore_import_plan importPlan_;
    std::set<std::string> existingIsoCodes_;
    std::vector<std::string> existingPortfolioNames_;

    int savedCurrencies_ = 0;
    int savedPortfolios_ = 0;
    int savedBooks_ = 0;
    int savedTrades_ = 0;
    bool importSuccess_ = false;
    QString importError_;
};

// ============================================================================
// Page declarations
// ============================================================================

class OreWelcomePage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreWelcomePage(OreImportWizard* wizard);
private:
    OreImportWizard* wizard_;
};

class OreDirectoryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_directory_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreDirectoryPage(OreImportWizard* wizard);
    bool isComplete() const override;
    bool validatePage() override;

private slots:
    void onBrowseClicked();
    void onScanFinished();

private:
    void startScan();

    OreImportWizard* wizard_;
    QLineEdit* dirEdit_;
    QPushButton* browseBtn_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    bool scanComplete_ = false;
    bool scanning_ = false;
};

class OreScanSummaryPage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreScanSummaryPage(OreImportWizard* wizard);
    void initializePage() override;

private slots:
    void onExclusionAdded();
    void onExclusionRemoved();

private:
    void refreshSummary();

    OreImportWizard* wizard_;
    QLabel* summaryLabel_;
    QListWidget* exclusionList_;
    QLineEdit* exclusionEdit_;
    QPushButton* addBtn_;
    QPushButton* removeBtn_;
    QLabel* hierarchyLabel_;
};

class OreCurrencyPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_currency_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreCurrencyPage(OreImportWizard* wizard);
    void initializePage() override;

private slots:
    void onFetchFinished();
    void onModeChanged();

private:
    OreImportWizard* wizard_;
    QRadioButton* allRadio_;
    QRadioButton* missingRadio_;
    QLabel* statusLabel_;
    QLabel* countLabel_;
    bool fetchDone_ = false;
};

class OrePortfolioPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_portfolio_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OrePortfolioPage(OreImportWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private slots:
    void onCreateParentToggled(bool checked);
    void onPortfoliosFetchFinished();

private:
    OreImportWizard* wizard_;
    QCheckBox* createParentCheck_;
    QComboBox* parentCombo_;
    QLabel* hierarchyPreviewLabel_;
    QRadioButton* addTradesRadio_;
    QRadioButton* newVersionsRadio_;
    bool fetchDone_ = false;
};

class OreTradeImportPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_trade_import_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreTradeImportPage(OreImportWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;
    bool validatePage() override;

private slots:
    void onImportFinished();

private:
    void startImport();
    void appendLog(const QString& msg);

    OreImportWizard* wizard_;
    QLineEdit* tradeDateEdit_;
    QLineEdit* lifecycleEventEdit_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool importDone_ = false;
    bool importStarted_ = false;
};

class OreDonePage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreDonePage(OreImportWizard* wizard);
    void initializePage() override;
private:
    OreImportWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
