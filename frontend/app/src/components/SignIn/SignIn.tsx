import { Box, Button, Grid } from '@material-ui/core';
import styled from '@emotion/styled';
import { AxiosError } from 'axios';
import { useQuery } from 'react-query';
import { GiChickenOven } from 'react-icons/gi';
import React, { useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { setWalletProvider } from '../../contracts/client';
import { APP_NAME, NETWORK } from '../../utils/globals';
import { getBeaconInstance } from '../../wallet';
import { useWallet } from '../../wallet/hooks';
import Identicon from '../Identicon';
import ProfilePopover from '../ProfilePopover';
import { UserBalance } from '../../interfaces';
import { getUserBalance } from '../../api/user';

const SignedInBoxStyled = styled(Box)`
  cursor: pointer;
`;

export const SignIn: React.FC = () => {
  const { t } = useTranslation(['header']);
  const [{ wallet, pkh: userAddress, network }, setWallet, disconnectWallet] = useWallet();
  const [isOpen, setOpen] = useState(false);
  const { data: balance } = useQuery<UserBalance | undefined, AxiosError, UserBalance | undefined>(
    [`user-balance-${userAddress}`],
    () => {
      if (userAddress) {
        return getUserBalance(userAddress);
      }
    },
  );
  const connectWallet = async () => {
    const newWallet = await getBeaconInstance(APP_NAME, true, NETWORK);
    newWallet?.wallet && setWalletProvider(newWallet.wallet);
    newWallet && setWallet(newWallet);
  };

  return (
    <div>
      <Grid container direction="row" style={{ flexWrap: 'nowrap' }}>
        <Box component="span" pr={1}>
          <Button
            variant="outlined"
            component={RouterLink}
            to="/create"
            endIcon={<GiChickenOven />}
            sx={{ textTransform: 'none' }}
          >
            {t('createOven')}
          </Button>
        </Box>
        {!wallet ? (
          <Box component="span">
            <Button variant="outlined" onClick={connectWallet} sx={{ textTransform: 'none' }}>
              {t('signIn')}
            </Button>
          </Box>
        ) : (
          <Grid item>
            <SignedInBoxStyled>
              <Identicon seed={userAddress ?? ''} onClick={() => setOpen(true)} type="tzKtCat" />
              <ProfilePopover
                isOpen={isOpen}
                onClose={() => setOpen(false)}
                handleAction={disconnectWallet}
                address={userAddress ?? ''}
                network={network ?? ''}
                actionText={t('signOut')}
                balance={balance}
              />
            </SignedInBoxStyled>
          </Grid>
        )}
      </Grid>
    </div>
  );
};
